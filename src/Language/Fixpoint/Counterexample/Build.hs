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
import Language.Fixpoint.SortCheck (Elaborate (..))

import qualified Language.Fixpoint.Utils.Files as Ext

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as Map
import Data.List (find, sortBy, foldl')

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
hornToProg
  :: MonadIO m
  => Fixpoint info
  => Show info
  => Config
  -> SInfo info
  -> m Prog
hornToProg cfg si = do
  -- Initial program is just an empty main.
  let initial = Prog
       { functions = Map.singleton mainName (Func [] [])
       , definitions = mempty
       }
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
  addDefinitions
  constraints <- reader $ cm . info
  mapM_ addHorn constraints
  get

addDefinitions :: MonadBuild info m => m ()
addDefinitions = do
  eqs <- reader $ aenvEqs . ae . info
  mapM_ addEquation eqs

addEquation :: MonadBuild info m => Equation -> m ()
addEquation equ = do
  -- Build a constraint from the equation
  let call = foldl' EApp (EVar $ eqName equ) (EVar . fst <$> eqArgs equ)
  let eq = PAtom Eq call $ eqBody equ
  constraint' <- elaborate' $ PAll (eqArgs equ) eq

  modify (\s -> s { definitions = constraint' : definitions s })

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
-- Ignore abstractions and functions, as they don't have refinements.
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
    sort' <- elaborate' sort
    let decl = Let $ Decl sym' sort'

    -- TODO: Could we perhaps remove duplicate calls here? There seem to be a
    -- lot, so this could eliminate a lot of overhead potentially!

    -- Get constraints from the expression.
    let constraints = exprStmts bid e
    -- Do substitution of self variable in the constraints
    let sub = Su $ Map.singleton v (EVar sym)
    return $ decl : subst sub constraints

-- | Split the expression into a number of statements
--
-- Note that kvars should only appear as conjuncted from the root expression,
-- or as the root itself. This function does not catch nested kvars.
exprStmts :: BindId -> Expr -> [Statement]
exprStmts bid = go
  where
    go (PAnd ps) = ps >>= go
    -- TODO: Change call so it doesn't take a list of locations.
    go (PKVar k su) = [Call bid [(k, su)]]
    go e = [Assume e]

-- | The sorts for the apply monomorphization only match if we do this elaborate
-- on the sort. Not sure why...
--
-- This elaboration also happens inside the declaration of the symbol
-- environment, so that's where I got the idea.
elaborate' :: MonadBuild info m => Elaborate a => a -> m a
elaborate' x = do
  symbols' <- reader symbols
  return $ elaborate "elaborateSort" symbols' x

-- | Add a function to the function map with index by its name.
-- If an entry already exists, it will merge the function
-- bodies.
addFunc :: MonadBuild info m => Name -> Func -> m ()
addFunc kvar func = do
  let merge (Func _ b) (Func d b') = Func d (b <> b')
  prog <- get
  let functions' = Map.insertWith merge kvar func $ functions prog
  put $ prog { functions = functions' }

-- | We try to place functions with as little kvars as possible first, as these
-- most likely find us a counterexample. Ideally, we do something less primitive
-- than just a sort though...
sortBodies :: Prog -> Prog
sortBodies prog = prog { functions = functions' }
  where
    functions' = sortFunc <$> functions prog

    sortFunc (Func sig bodies) = Func sig $ sortBy cmp bodies
    cmp a b = count a `compare` count b
    count (Body _ stmts) = length . filter isCall $ stmts
    isCall (Call _ _) = True
    isCall _ = False
