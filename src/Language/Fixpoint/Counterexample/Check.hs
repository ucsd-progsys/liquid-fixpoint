{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample.Check
  ( checkProg
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.Counterexample.Types
import Language.Fixpoint.Counterexample.SMT as SMT
import Language.Fixpoint.Types.Config (Config)

import Data.Maybe (fromJust, catMaybes)
import Data.List (find)
import qualified Data.HashMap.Strict as Map

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad (forM, foldM, when)

-- | Environment for the counter example generation.
data CheckEnv = CheckEnv
  { program :: !Prog
  -- ^ The program we are checking
  , context :: !SMT.Context
  -- ^ The SMT context we write the constraints from the program to.
  , maxDepth :: !Int
  -- ^ The maximum number of functions to traverse (to avoid state blow-up).
  }

instance SMTContext CheckEnv where
  smtContext = context

-- | The monad used to generate counter examples from a Prog.
type MonadCheck m = (MonadReader CheckEnv m, MonadIO m)

-- | The runner is a computation path in the program. We use this as an argument
-- to pass around the remainder of a computation. This way, we can pop paths in
-- the SMT due to conditionals. Allowing us to retain anything prior to that.
type Runner m = m (Maybe SMTCounterexample)

-- | Check the given constraints to try and find a counter example.
checkProg :: MonadIO m => Config -> SInfo info -> Prog -> [SubcId] -> m [SMTCounterexample]
checkProg cfg si prog cids = SMT.withContext cfg si check
  where
    check ctx = runCheck cids CheckEnv
      { program = prog
      , context = ctx
      -- TODO: Perhaps the max depth should be a parameter for the user?
      , maxDepth = 10
      }

-- | Runs the program checker with the monad stack
-- unwrapped.
runCheck :: MonadIO m => [SubcId] -> CheckEnv -> m [SMTCounterexample]
runCheck cids env = rd $ checkAll cids
  where
    rd = flip runReaderT env

-- | Try to find a counter example for all the given constraints.
checkAll :: MonadCheck m => [SubcId] -> m [SMTCounterexample]
checkAll cids = do
  -- Using definitions as quantified axioms is really slow! Perhaps this should
  -- be feature gated, or we should just never do this as there are most likely
  -- faster alternatives. I'll leave it like this for now.
  when False setDefinitions
  cexs <- forM cids checkConstraint
  return $ catMaybes cexs

setDefinitions :: MonadCheck m => m ()
setDefinitions = do
  defs <- reader $ definitions . program
  mapM_ SMT.assume defs

-- | Check a specific constraint id. This will only do actual
-- checks for constraints without a KVar on the rhs, as we cannot
-- really generate a counter example for these constraints.
checkConstraint :: MonadCheck m => SubcId -> m (Maybe SMTCounterexample)
checkConstraint cid = do
  Func _ bodies <- fromJust <$> getFunc mainName
  let cmp (Body bid _) = bid == cid
  let scope = Scope mempty cid mempty
  case find cmp bodies of
    Just body -> runBody scope body SMT.getModel
    Nothing -> return Nothing

-- | Run a function. This essentially makes one running branch for
-- each body inside of the function. It will try each branch
-- sequentially, returning early if a counterexample was found.
runFunc :: MonadCheck m => Name -> Scope -> Runner m -> Runner m
runFunc name scope runner = do
  -- Lookup function bodies
  func <- getFunc name
  maxDepth' <- reader maxDepth
  sat <- SMT.checkSat
  case func of
    -- This sub tree is already unsatisfiable. Adding more constraints never
    -- makes it satisfiable and, as such, we prune this subtree.
    _ | not sat -> return Nothing

    -- Recursion limit reached, so no counterexample.
    _ | length (path scope) >= maxDepth' -> return Nothing

    -- Unconstrained function body, so there is no counterexample here. This
    -- would be equivalent to trying to create an inhabitant of {v:a | false},
    -- which doesn't exist.
    Nothing -> return Nothing

    -- Constrained function body
    Just (Func _ bodies) -> do
      -- Generate all execution paths (as runners).
      let runner' body = runBody (extendScope scope body) body runner
      let paths = runner' <$> bodies

      -- TODO: We should really explore shallow trees first. The current thing
      -- can search for a really long time if there are a large number of paths
      -- before the actual counterexample...
      result <- foldRunners paths
      return result

-- | Run the statements in the body. If there are no more statements to run,
-- this will execute the Runner that was passed as argument.
--
-- The passed runner here is thus the rest of the computation, when we "return"
-- from this function.
runBody :: MonadCheck m => Scope -> Body -> Runner m  -> Runner m
runBody scope' body@(Body _ _) runner = SMT.inScope $ go scope' body
  where
    go _ (Body _ []) = runner
    go scope (Body cid (stmt:ss)) = do
      -- The remaining statements becomes a new Runner
      let runner' = flip go (Body cid ss)
      -- We pass this runner, such that it can be called at a later
      -- point (possibly multiple times) if we were to encounter a call.
      runStatement scope stmt runner'

-- | Run the current statement. It might adjust the substitution map
-- for a portion of the statements, which is why the runner takes a
-- new substitution map as an argument.
runStatement :: MonadCheck m => Scope -> Statement -> (Scope -> Runner m) -> Runner m
runStatement scope stmt runner = do
  -- Runner with the old subst map.
  let runner' = runner scope
  let stmt' = subst (binders scope) stmt
  case stmt' of
    Call origin calls -> do
      -- We fake a SubcId here, it will later get mapped into the scope when we
      -- decide which body to run.
      let scope' app = Scope (origin:path scope) 0 app
      let runCall (name, app) = runFunc name (scope' app) runner'
      foldRunners $ runCall <$> calls
    Assume e -> SMT.assume e >> runner'
    Assert e -> SMT.assert e >> runner'
    Let decl -> do
      scope' <- SMT.declare scope decl
      runner scope'

-- | Get a function from the program given its name.
getFunc :: MonadCheck m => Name -> m (Maybe Func)
getFunc name = do
  funcs <- reader $ functions . program
  return $ Map.lookup name funcs

-- | Fold the runners, selecting the first one that produced a counterexample,
-- if any.
foldRunners :: MonadCheck m => [Runner m] -> Runner m
foldRunners = foldM select Nothing
  where
    select Nothing r = r
    select cex _ = return cex

-- | Extend the scope to include the id of the body, i.e. the `SubcId`.
extendScope :: Scope -> Body -> Scope
extendScope scope (Body cid _) = scope { constraint = cid }
