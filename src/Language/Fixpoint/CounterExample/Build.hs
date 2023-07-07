{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.CounterExample.Build
  ( hornToProg
  ) where


import Language.Fixpoint.Types
import Language.Fixpoint.CounterExample.Types
import Language.Fixpoint.Types.Config (Config, queryFile, save)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)
import Language.Fixpoint.Misc (ensurePath)
import Language.Fixpoint.SortCheck (elaborate)

import qualified Language.Fixpoint.Utils.Files as Ext
import qualified Text.PrettyPrint.HughesPJ as PP

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as Map

import Control.Monad.State
import Control.Monad.Reader

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

  -- Run monad that adds all horn clauses
  prog <- evalStateT (runReaderT buildProg env) initial

  -- Save the program in a file
  liftIO . when (save cfg) $ do
    let file = queryFile Ext.Prog cfg
    ensurePath file
    writeFile file $ PP.render (pprint prog)

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
  let body = Body cid $ lhs <> rhs
  addFunc name $ Func decl [body]

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
  let lhs = clhs bindEnv horn
  stmts <- forM lhs $ uncurry reftToStmts
  return $ mconcat stmts

-- | Map a refinement to a declaration and constraint pair
reftToStmts :: MonadBuild info m => Symbol -> SortedReft -> m [Statement]
reftToStmts _ RR { sr_sort = FFunc _ _ } = return []
reftToStmts sym RR
  { sr_sort = sort
  , sr_reft = Reft (v, e)
  } = do
    -- Get correct sort for declaration
    sort' <- elaborateSort sort
    let decl = Let $ Decl sym sort'

    -- Get constraints from the expression.
    let constraints = case predKs e of
          [] -> [Assume e]
          ks -> map (uncurry $ Call sym) ks

    -- Do proper substitution of v in the constraints
    let sub = Su $ Map.singleton v (EVar sym)
    return $ decl : subst sub constraints

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