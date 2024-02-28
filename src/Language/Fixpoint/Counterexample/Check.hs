{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample.Check
  ( checkProg
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.Counterexample.Types
import Language.Fixpoint.Counterexample.SMT
import Language.Fixpoint.Types.Config (Config, srcFile)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)
import qualified Language.Fixpoint.Smt.Interface as SMT

import Data.Maybe (fromJust)
import Data.List (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Monad.State
import Control.Monad.Reader

-- | Multiple counter examples indexed per constraint id.
type SMTCounterexamples = HashMap SubcId SMTCounterexample

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

-- | State tracked when checking a program.
newtype CheckState = CheckState
  { depth :: Int
  -- ^ Current depth (i.e. number of functions traversed)
  }

-- | The monad used to generate counter examples from a Prog.
type MonadCheck m = (MonadReader CheckEnv m, MonadState CheckState m, MonadIO m)

-- | Check the given constraints to try and find a counter example.
checkProg :: MonadIO m => Config -> SInfo info -> Prog -> [SubcId] -> m SMTCounterexamples
checkProg cfg si prog cids = withContext cfg si check
  where
    check ctx = runCheck cids CheckEnv
      { program = prog
      , context = ctx
      , maxDepth = 7 -- TODO: Perhaps this should be a parameter for the user?
      }

-- | Run the checker with the SMT solver context.
withContext :: MonadIO m => Config -> SInfo info -> (SMT.Context -> m a) -> m a
withContext cfg si inner = do
  let file = srcFile cfg <> ".prog"
  let env = symbolEnv cfg si
  ctx <- liftIO $ SMT.makeContextWithSEnv cfg file env

  !result <- inner ctx

  liftIO $ SMT.cleanupContext ctx
  return result

-- | Runs the program checker with the monad stack
-- unwrapped.
runCheck :: MonadIO m => [SubcId] -> CheckEnv -> m SMTCounterexamples
runCheck cids env = rd . st $ checkAll cids
  where
    st = flip evalStateT $ CheckState 0
    rd = flip runReaderT env

-- | Try to find a counter example for all the given constraints.
checkAll :: MonadCheck m => [SubcId] -> m SMTCounterexamples
checkAll cids = do
  cexs <- forM cids checkConstraint
  return $ Map.fromList [(cid, cex) | (cid, Just cex) <- zip cids cexs]

-- | Check a specific constraint id. This will only do actual
-- checks for constraints without a KVar on the rhs, as we cannot
-- really generate a counter example for these constraints.
checkConstraint :: MonadCheck m => SubcId -> m (Maybe SMTCounterexample)
checkConstraint cid = do
  Func _ bodies <- fromJust <$> getFunc mainName
  let cmp (Body bid _) = bid == cid
  let scope = Scope mempty mempty
  case find cmp bodies of
    Just body -> runBody scope body smtCheck
    Nothing -> return Nothing

-- | Run a function. This essentially makes one running branch for
-- each body inside of the function. It will try each branch
-- sequentially, returning early if a counterexample was found.
runFunc :: MonadCheck m => Name -> Scope -> Runner m -> Runner m
runFunc name scope runner = do
  -- Lookup function bodies
  func <- getFunc name
  case func of
    -- Unconstrained function body, so we just continue with the runner.
    Nothing -> runner
    -- Constrained function body
    Just (Func _ bodies) -> do
      -- Generate all execution paths (as runners).
      let runner' body = runBody (extendScope scope body) body runner
      let paths = runner' <$> bodies

      -- Try paths, selecting the first that produces a counter example.
      let select Nothing r = r
          select cex _ = return cex

      -- Check if we've reached the recursion limit.
      -- TODO: Perhaps we should make the recursion limit per kvar?
      -- TODO: We should really explore shallow trees first. Right now,
      -- we explore max depth trees only...
      depth' <- gets depth
      put $ CheckState $ depth' + 1
      maxDepth' <- reader maxDepth
      let recursionLimit = depth' >= maxDepth'

      result <- if recursionLimit then runner else foldM select Nothing paths

      -- Decrement depth after exploring this function
      modify $ \s -> s { depth = depth s - 1}
      return result

-- | Extend the scope to include the id of the body, i.e. the `SubcId`.
extendScope :: Scope -> Body -> Scope
extendScope scope (Body bodyId _) = withSubcId scope
  where
    withSubcId scope'@Scope { path = (bindId,_):ps } = scope' { path = (bindId, bodyId):ps }
    withSubcId _ = error "No scope to extend."

-- | Run the statements in the body. If there are no more statements to run,
-- this will execute the Runner that was passed as argument.
--
-- The passed runner here is thus the rest of the computation, when we "return"
-- from this function.
runBody :: MonadCheck m => Scope -> Body -> Runner m  -> Runner m
runBody scope' body runner = smtScope $ go scope' body
  where
    go _ (Body _ []) = runner
    go scope (Body cid (stmt:ss)) = do
      -- The remaining statements become a new Runner
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
    Call origin name app -> do
      -- We fake a SubcId here, it will later get mapped into the scope when we
      -- decide which body to run.
      let scope' = Scope ((origin, 0):path scope) app
      runFunc name scope' runner'
    Assume e -> smtAssume e >> runner'
    Assert e -> smtAssert e >> runner'
    Let decl -> do
      scope' <- smtDeclare scope decl
      runner scope'

-- | Get a function from the program given its name.
getFunc :: MonadCheck m => Name -> m (Maybe Func)
getFunc name = do
  Prog prog <- reader program
  return $ Map.lookup name prog
