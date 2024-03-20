{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample.Build
  ( hornToProg
  ) where


import Language.Fixpoint.Types
import Language.Fixpoint.Counterexample.Types
import Language.Fixpoint.Types.Config (Config, queryFile, save)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)
import Language.Fixpoint.Misc (ensurePath)
import Language.Fixpoint.SortCheck (elaborate)

import qualified Language.Fixpoint.Utils.Files as Ext

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as Map
import Data.List (find, sortBy)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (when, forM)

-- | The enviroment used to build a program.
data BuildEnv info = BuildEnv
  { info :: !(SInfo info)
  -- ^ The horn constraints from which we build the program.
  , symbols :: !SymEnv
  -- ^ Contains the sorts of symbols, which we need for declarations.
  }

-- | The monad used to convert a set of horn constraints to
-- the imperative function format. See Prog for the format.
type MonadBuild info m = (MonadReader (BuildEnv info) m, MonadState Prog m, MonadIO m)

type Binding = (BindId, Symbol, SortedReft)

-- | Make an imperative program from horn clauses. This
-- can be used to generate a counter example.
hornToProg :: MonadIO m => Config -> SInfo info -> m Prog
hornToProg cfg si = do
  -- Initial program is just an empty main.
  let initial = Prog $ Map.singleton mainName (Func [] [])
  let env = BuildEnv
       { info = si
       , symbols = symbolEnv cfg si
       }

  -- Run monad that adds all horn clauses to the program
  prog <- sortBodies <$> evalStateT (runReaderT buildProg env) initial

  -- Save the program in a file
  liftIO . when (save cfg) $ do
    let file = queryFile Ext.Prog cfg
    liftIO . putStrLn $ "Saving counterexample program: " ++ file ++ "\n"
    ensurePath file
    writeFile file . show . pprint $ prog

  -- Return the generated program
  return prog

-- | Build the entire program structure from the constraints
-- inside the monad
buildProg :: MonadBuild info m => m Prog
buildProg = do
  constraints <- reader $ cm . info
  mapM_ addHorn constraints
  get

-- | Given a horn clause, generates a body for a function.
--
-- The body is generated from the lhs of the horn clause.
--
-- This body is added to the function given by the kvar
-- on the rhs of the horn clause. If there was no kvar,
-- it is added to the main function.
addHorn :: MonadBuild info m => SimpC info -> m ()
addHorn horn = do
  -- Make the lhs of the clause into statements
  lhs <- hornLhsToStmts horn

  -- The rhs has a special case depending on
  -- if it is a kvar or not.
  (name, decl, rhs) <- case crhs horn of
    PKVar name sub -> do
      decl <- getSig name
      rhs <- substToStmts sub
      return (name, decl, rhs)
    e -> return (mainName, [], [Assert e])

  -- Add the horn clause as a function body
  let cid = fromMaybe (-1) $ sid horn
  let statements = sortStatements $ lhs <> rhs
  let body = Body cid $ statements
  addFunc name $ Func decl [body]

-- | Sort the statements so we do all declarations first.
-- TODO: Change the `Body` type so it contains a substitution map. Remove the
-- Let statement from the types of statements we have!
sortStatements :: [Statement] -> [Statement]
sortStatements = sortBy cmp
  where
    cmp (Let _) (Let _) = EQ
    cmp (Let _) _ = LT
    cmp _ (Let _) = GT
    cmp _ _ = EQ

-- | Gets a signature of a KVar from its well foundedness constraint
getSig :: MonadBuild info m => Name -> m Signature
getSig kvar = do
  -- Get the well foundedness constraint of the kvar
  wfcs <- reader $ ws . info
  let wfc = wfcs Map.! kvar

  -- Get the bind environment and bindings of the wfc
  bindEnv <- reader $ bs . info
  let ibinds = elemsIBindEnv . wenv $ wfc

  -- Lookup all Decl from the wfc using the ibinds
  let asDecl (sym, sr, _) = Decl sym (sr_sort sr)
  let decls = map (asDecl . flip lookupBindEnv bindEnv) ibinds

  -- Get the last Decl from the head of the wfc
  let rhs = let (sym, sort, _) = wrft wfc in Decl sym sort

  -- Return the head + bindings as argument map
  return $ rhs:decls

-- | Defines some equalities between local variables
-- and the passed arguments given some substitution map.
substToStmts :: MonadBuild info m => Subst -> m [Statement]
substToStmts (Su sub) = do
  let asEq (ksym, e) = Assume $ PAtom Eq (EVar ksym) e
  return $ map asEq (Map.toList sub)

-- | Converts the left hand side of the horn clause to a list
-- of assumptions (or calls given by a Name)
hornLhsToStmts :: MonadBuild info m => SimpC info -> m [Statement]
hornLhsToStmts horn = do
  bindEnv <- reader $ bs . info
  let lhs = relevantLhs bindEnv horn
  lhs' <- filterLits . filterDuplicates $ lhs
  stmts <- forM lhs' reftToStmts
  return $ mconcat stmts

relevantLhs :: BindEnv info -> SimpC info -> [Binding]
relevantLhs benv horn = 
  [ (bid, sym, ref) 
  | bid <- elemsIBindEnv $ senv horn
  , let (sym, ref, _) = lookupBindEnv bid benv
  ]

filterDuplicates :: [Binding] -> [Binding]
filterDuplicates = foldr filter' []
  where
    filter' e acc = case e `member` acc of
      Nothing -> e:acc
      Just _ -> acc

    snd' (_, x, _) = x
    member e es = find (snd' e==) $ map snd' es

filterLits :: MonadBuild info m => [Binding] -> m [Binding]
filterLits env = do
  con <- reader $ gLits . info
  dis <- reader $ dLits . info
  let isLit (_, sym, _) = memberSEnv sym con || memberSEnv sym dis
  return $ filter (not . isLit) env

-- | Map a refinement to a declaration and constraint pair
reftToStmts :: MonadBuild info m => Binding -> m [Statement]
reftToStmts (_, _, RR { sr_sort = FAbs _ _ }) = return []
reftToStmts (_, _, RR { sr_sort = FFunc _ _ }) = return []
reftToStmts (bid, sym, RR
  { sr_sort = sort
  , sr_reft = Reft (v, e)
  }) = do
    -- Prefix the symbol if required. Otherwise, some symbols won't match their
    -- fix$36$ version when substituting.
    let sym' = symbol . prefixAlpha . symbolText $ sym

    -- Get correct sort for declaration
    sort' <- elaborateSort sort
    let decl = Let $ Decl sym' sort'

    -- Get constraints from the expression.
    let constraints = case predKs e of
          [] -> Assume e
          ks -> Call bid ks

    -- Do substitution of self variable in the constraints
    let sub = Su $ Map.singleton v (EVar sym)
    return [decl, subst sub constraints]

-- | Get the kvars from an expression.
--
-- I think this should be the only way in which kvars appear?
-- Otherwise, this should be changed!
predKs :: Expr -> [(KVar, Subst)]
predKs (PAnd ps)    = mconcat $ map predKs ps
predKs (PKVar k su) = [(k, su)]
predKs _            = []

-- | The sorts for the apply monomorphization only match if
-- we do this elaborate on the sort. Not sure why...
--
-- This elaboration also happens inside the declaration
-- of the symbol environment, so that's where I got the idea.
elaborateSort :: MonadBuild info m => Sort -> m Sort
elaborateSort sym = do
  symbols' <- reader symbols
  return $ elaborate "elaborateSort" symbols' sym

-- | Add a function to the function map with index by its name.
-- If an entry already exists, it will merge the function
-- bodies.
addFunc :: MonadBuild info m => Name -> Func -> m ()
addFunc kvar func = do
  let merge (Func _ b) (Func d b') = Func d (b <> b')
  Prog prog <- get
  put . Prog $ Map.insertWith merge kvar func prog

-- | We try to place functions with as little kvars as possible first, as these
-- most likely find us a counterexample. Ideally, we do something less primitive
-- than just a sort though...
sortBodies :: Prog -> Prog
sortBodies (Prog prog) = Prog $ sortFunc <$> prog
  where
    sortFunc (Func sig bodies) = Func sig $ sortBy cmp bodies
    cmp a b = count a `compare` count b
    count (Body _ stmts) = length . filter isCall $ stmts
    isCall (Call _ _) = True
    isCall _ = False
