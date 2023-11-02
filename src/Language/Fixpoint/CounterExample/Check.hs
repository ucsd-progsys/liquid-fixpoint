{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.CounterExample.Check
  ( checkProg
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.CounterExample.Types
import Language.Fixpoint.Types.Config (Config, srcFile)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)
import qualified Language.Fixpoint.Smt.Interface as SMT

import Data.Char (chr)
import Data.List (find, intercalate, foldl')
import Data.Bifunctor (first)
import Data.String (IsString(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Control.Monad.State
import Control.Monad.Reader

-- | Multiple counter examples indexed per constraint id.
type CounterExamples = HashMap SubcId CounterExample

-- | Environment for the counter example generation.
data CheckEnv = CheckEnv
  { program :: !Prog
  -- ^ The program we are checking
  , context :: !SMT.Context
  -- ^ The SMT context we write the constraints from the program to.
  , maxDepth :: !Int
  -- ^ The maximum number of functions to traverse (to avoid state blow-up).
  }

-- | State tracked when checking a program.
newtype CheckState = CheckState
  { depth :: Int
  -- ^ Current depth (i.e. number of functions traversed)
  }

-- | The monad used to generate counter examples from a Prog.
type MonadCheck m = (MonadReader CheckEnv m, MonadState CheckState m, MonadIO m)

-- | Check the given constraints to try and find a counter example.
checkProg :: MonadIO m => Config -> SInfo info -> Prog -> [SubcId] -> m CounterExamples
checkProg cfg si prog cids = withContext cfg si check
  where
    check ctx = runCheck cids CheckEnv
      { program = prog
      , context = ctx
      , maxDepth = 100 -- TODO: Perhaps this should be a parameter for the user?
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
runCheck :: MonadIO m => [SubcId] -> CheckEnv -> m CounterExamples
runCheck cids env = rd . st $ checkAll cids
  where
    st = flip evalStateT $ CheckState 0
    rd = flip runReaderT env

-- | Try to find a counter example for all the given constraints.
checkAll :: MonadCheck m => [SubcId] -> m CounterExamples
checkAll cids = do
  cexs <- forM cids checkConstraint
  return $ Map.fromList [(cid, cex) | (cid, Just cex) <- zip cids cexs]

-- | Check a specific constraint id. This will only do actual
-- checks for constraints without a KVar on the rhs, as we cannot
-- really generate a counter example for these constraints.
checkConstraint :: MonadCheck m => SubcId -> m (Maybe CounterExample)
checkConstraint cid = do
  Func _ bodies <- getFunc mainName
  let cmp (Body bid _) = bid == cid
  let scope = Scope mempty mempty
  case find cmp bodies of
    Just body -> runBody scope body smtCheck
    Nothing -> return Nothing

-- | A scope contains the current binders in place
-- as well as the path traversed to reach this scope.
data Scope = Scope
  { path :: ![Symbol]
  -- ^ The path traversed to reach the scope.
  , binders :: !Subst
  -- ^ The binders available in the current scope.
  }
  deriving (Eq, Ord, Show)

-- | The runner is a computation path in the program. We use this
-- as an argument to pass around the remainder of a computation.
-- This way, we can pop paths in the SMT due to conditionals.
-- Allowing us to retain anything prior to that.
type Runner m = m (Maybe CounterExample)

-- | Run a function. This essentially makes one running branch for
-- each body inside of the function. It will try each branch
-- sequentially, returning early if a counterexample was found.
runFunc :: MonadCheck m => Name -> Scope -> Runner m -> Runner m
runFunc name scope runner = do
  -- Lookup function bodies
  Func _ bodies <- getFunc name

  -- Generate all execution paths (as runners).
  let runner' body = runBody scope body runner
  let paths = map runner' bodies

  -- Try paths, selecting the first that produces a counter example.
  let select Nothing r = r
      select cex _ = return cex

  -- Check if we've reached the recursion limit.
  -- TODO: Perhaps we should make the recursion limit per kvar?
  depth' <- gets depth
  put $ CheckState $ depth' + 1
  maxDepth' <- reader maxDepth
  let recursionLimit = depth' >= maxDepth'

  result <- if recursionLimit then runner else foldM select Nothing paths

  -- Decrement depth after exploring this function
  modify $ \s -> s { depth = depth s - 1}
  return result

-- | Run the statements in the body. If there are no more statements
-- to run, this will execute the Runner that was passed as argument.
--
-- The passed runner here is thus the rest of the computation, when
-- we "return" from this function.
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
      let scope' = Scope (origin:path scope) app
      runFunc name scope' runner'
    Assume e -> smtAssume e >> runner'
    Assert e -> smtAssert e >> runner'
    Let decl -> smtDeclare scope decl >>= runner -- Run with modified scope.

-- | Get a function from the program given its name.
getFunc :: MonadCheck m => Name -> m Func
getFunc name = do
  Prog prog <- reader program
  return $ prog Map.! name

-- | Returns a version of a symbol with the scope encoded into its name.
scopedSym :: MonadCheck m => Scope -> Symbol -> m Symbol
scopedSym scope sym = do
  let strs = symbolString <$> progPrefix : sym : path scope
  let name = intercalate bindSep strs
  return $ symbol name

-- | Declare a new symbol, returning an updated substitution
-- given with this new symbol in it. The substitution map is
-- required to avoid duplicating variable names.
smtDeclare :: MonadCheck m => Scope -> Decl -> m Scope
smtDeclare scope@Scope { binders = Su binds } (Decl sym _)
  | Map.member sym binds = return scope
smtDeclare scope (Decl sym sort) = do
  ctx <- reader context
  sym' <- scopedSym scope sym
  liftIO $ SMT.smtDecl ctx sym' sort
  let Su sub = binders scope
  let binders' = Su $ Map.insert sym (EVar sym') sub
  return scope { binders = binders' }

-- | Assume the given expression.
smtAssert :: MonadCheck m => Expr -> m ()
smtAssert = smtAssume . PNot

-- | Assert the given expression.
smtAssume :: MonadCheck m => Expr -> m ()
smtAssume e = do
  ctx <- reader context
  liftIO $ SMT.smtAssert ctx e

-- | Run the checker within a scope (i.e. a push/pop pair).
smtScope :: MonadCheck m => m a -> m a
smtScope inner = do
  ctx <- reader context
  liftIO $ SMT.smtPush ctx
  !result <- inner
  liftIO $ SMT.smtPop ctx
  return result

-- | Check if there is a counterexample, returing one
-- if it is available.
smtCheck :: MonadCheck m => Runner m
smtCheck = do
  ctx <- reader context
  valid <- liftIO $ SMT.smtCheckUnsat ctx

  if valid then return Nothing else Just <$> smtModel

-- | Returns a model, with as precondition that the SMT 
-- solver had a satisfying assignment prior to this.
smtModel :: MonadCheck m => m CounterExample
smtModel = do
  ctx <- reader context
  Su sub <- liftIO $ SMT.smtGetModel ctx

  -- Filter just the variables for which we have a trace
  let renames = first symbolTrace <$> Map.toList sub
  let traces = [ (trace, sym, e) | (Just (sym, trace), e) <- renames ]

  -- Insert a mapping per unique layer in the counter example.
  let new sym e = Su $ Map.singleton sym e
  let insert cex (trace, sym, e) = Map.insertWith (<>) trace (new sym e) cex
  let cex = foldl' insert mempty traces
  return cex

-- | We encode the trace of a symbol in its name. This way,
-- we do not lose it in the SMT solver. This function translates
-- the encoding back.
symbolTrace :: Symbol -> Maybe (Symbol, [Symbol])
symbolTrace sym = case T.splitOn bindSep sym' of
  (prefix:name:trace) | prefix == progPrefix 
    -> Just (symbol name, symbol <$> trace)
  _ -> Nothing
  where
    sym' = escapeSmt . symbolText $ sym

escapeSmt :: T.Text -> T.Text
escapeSmt = go False . T.split (=='$')
  where
    go _ [] = ""
    go escape (t:ts) = txt t <> go (not escape) ts
      where
        txt | escape    = T.singleton . chr . read . T.unpack
            | otherwise = id

-- | The separator used to encode the stack trace (of binders)
-- inside of smt symbols.
bindSep :: IsString a => a
bindSep = "@"

-- | Prefix used to show that this smt symbol was generated
-- during a run of the program.
progPrefix :: IsString a => a
progPrefix = "prog"
