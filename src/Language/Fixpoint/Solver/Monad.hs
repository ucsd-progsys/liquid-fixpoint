{-# LANGUAGE OverloadedStrings #-}

-- | This is a wrapper around IO that permits SMT queries

module Language.Fixpoint.Solver.Monad
       ( -- * Type
         SolveM
       , liftSMT

         -- * Execution
       , runSolverM

         -- * Get Binds
       , getBinds
       , getContext

         -- * SMT Query
       , filterRequired
       , filterValid
       , filterValidGradual
       , checkSat
       , smtEnablembqi
       , sendConcreteBindingsToSMT

         -- * Debug
       , Stats
       , tickIter
       , stats
       , numIter
       , SolverState(..)

       , modifyContext
       , clearApplys
       )
       where

import           Control.Monad (foldM, forM, forM_, when)
import           Language.Fixpoint.Utils.Progress
import qualified Language.Fixpoint.Types.Config  as C
import           Language.Fixpoint.Types.Config  (Config)
import qualified Language.Fixpoint.Types   as F
-- import qualified Language.Fixpoint.Misc    as Misc
-- import           Language.Fixpoint.SortCheck
import qualified Language.Fixpoint.Types.Solutions as F
import qualified Language.Fixpoint.Types.Visitor as F
-- import qualified Language.Fixpoint.Types.Errors  as E
import           Language.Fixpoint.Smt.Serialize ()
import           Language.Fixpoint.Types.PrettyPrint ()
import           Language.Fixpoint.Smt.Interface
import           Language.Fixpoint.Smt.Types (SmtM)
-- import qualified Language.Fixpoint.Smt.Theories as Thy
import           Language.Fixpoint.Solver.Sanitize
import           Language.Fixpoint.Solver.Stats
import           Language.Fixpoint.Graph.Types (SolverInfo (..))
-- import           Language.Fixpoint.Solver.Solution
-- import           Data.Maybe           (catMaybes)
import           Data.List            (partition)
-- import           Data.Char            (isUpper)
import qualified Control.Monad.State as ST
import           Control.Monad.State.Strict
--import           Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import           Data.Maybe (catMaybes)
import           Control.Exception.Base (bracket)

--import Debug.Trace

--------------------------------------------------------------------------------
-- | Solver Monadic API --------------------------------------------------------
--------------------------------------------------------------------------------

type SolveM ann = StateT (SolverState ann) IO

data SolverState ann = SS
  { ssCtx     :: !Context         -- ^ SMT Solver Context
  , ssBinds   :: !(F.BindEnv ann) -- ^ All variables and types
  , ssStats   :: !Stats           -- ^ Solver Statistics
  }

stats0    :: F.GInfo c b -> Stats
stats0 fi = Stats nCs 0 0 0 0
  where
    nCs   = M.size $ F.cm fi

--------------------------------------------------------------------------------
runSolverM :: Config -> SolverInfo ann c -> SolveM ann a -> IO a
--------------------------------------------------------------------------------
runSolverM cfg sI act =
  bracket acquire release $ \ctx -> do
    res <- runStateT act' (s0 ctx)
    return (fst res)
  where
    s0 ctx   = SS ctx be (stats0 fi)
    act'     = assumesAxioms (F.asserts fi) >> act
    release  = cleanupContext
    acquire  = makeContextWithSEnv cfg file initEnv (F.defns fi)
    initEnv  = symbolEnv cfg fi
    be       = F.bs fi
    file     = C.srcFile cfg
    -- only linear arithmetic when: linear flag is on or solver /= Z3
    -- lar     = linear cfg || Z3 /= solver cfg
    fi       = (siQuery sI) {F.hoInfo = F.cfgHoInfo cfg }


--------------------------------------------------------------------------------
getBinds :: SolveM ann (F.BindEnv ann)
--------------------------------------------------------------------------------
getBinds = ssBinds <$> get

--------------------------------------------------------------------------------
getIter :: SolveM ann Int
--------------------------------------------------------------------------------
getIter = numIter . ssStats <$> get

--------------------------------------------------------------------------------
incIter, incBrkt :: SolveM ann ()
--------------------------------------------------------------------------------
incIter   = modifyStats $ \s -> s {numIter = 1 + numIter s}
incBrkt   = modifyStats $ \s -> s {numBrkt = 1 + numBrkt s}

--------------------------------------------------------------------------------
incChck, incVald :: Int -> SolveM ann ()
--------------------------------------------------------------------------------
incChck n = modifyStats $ \s -> s {numChck = n + numChck s}
incVald n = modifyStats $ \s -> s {numVald = n + numVald s}

liftSMT :: SmtM a -> SolveM ann a
liftSMT k =
  do es <- get
     let ctx = ssCtx es
     (a, ctx') <- lift $ ST.runStateT k ctx
     put (es {ssCtx = ctx'})
     pure a

getContext :: SolveM ann Context
getContext = ssCtx <$> get

modifyStats :: (Stats -> Stats) -> SolveM ann ()
modifyStats f = modify $ \s -> s { ssStats = f (ssStats s) }

modifyContext :: (Context -> Context) -> SolveM ann ()
modifyContext f = modify $ \s -> s { ssCtx = f (ssCtx s) }

clearApplys :: SolveM ann ()
clearApplys = modifyContext $ \c -> c { ctxSymEnv = (ctxSymEnv c) { F.seAppls = mempty , F.seApplsCur = mempty , F.seIx = 0 } }

--------------------------------------------------------------------------------
-- | SMT Interface -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Takes the environment of bindings already known to the SMT,
-- and the environment of all bindings that need to be known.
--
-- Yields the ids of bindings known to the SMT
sendConcreteBindingsToSMT
  :: F.IBindEnv -> (F.IBindEnv -> SolveM ann a) -> SolveM ann a
sendConcreteBindingsToSMT known act = do
  be <- getBinds
  let concretePreds =
        [ (i, F.subst1 p (v, F.EVar s))
        | (i, (s, F.RR _ (F.Reft (v, p)),_)) <- F.bindEnvToList be
        , F.isConc p
        , not (isShortExpr p)
        , not (F.memberIBindEnv i known)
        ]
  st <- get
  (a, st'') <- liftSMT $
    smtBracket "sendConcreteBindingsToSMT" $ do
      forM_ concretePreds $ \(i, e) ->
        smtDefineFunc (F.bindSymbol (fromIntegral i)) [] F.boolSort e
      ctx <- get
      let st' = st { ssCtx = ctx }
      liftIO $ flip runStateT st' $ act $ F.unionIBindEnv known $ F.fromListIBindEnv $ map fst concretePreds
  put st''
  return a
  where
    isShortExpr F.PTrue = True
    isShortExpr F.PTop = True
    isShortExpr _ = False

-- | `filterRequired [(x1, p1),...,(xn, pn)] q` returns a minimal list [xi] s.t.
--   /\ [pi] => q
--------------------------------------------------------------------------------
filterRequired :: F.Cand a -> F.Expr -> SolveM ann [a]
--------------------------------------------------------------------------------
filterRequired = error "TBD:filterRequired"

--------------------------------------------------------------------------------
-- | `filterValid p [(q1, x1),...,(qn, xn)]` returns the list `[ xi | p => qi]`
--------------------------------------------------------------------------------
{-# SCC filterValid #-}
filterValid :: F.SrcSpan -> F.Expr -> F.Cand a -> SolveM ann [a]
--------------------------------------------------------------------------------
filterValid sp p qs = do
  qs' <- liftSMT $
           smtBracket "filterValidLHS" $
             filterValid_ sp p qs
--  clearApplys
  -- stats
  incBrkt
  incChck (length qs)
  incVald (length qs')
  return qs'

{-# SCC filterValid_ #-}
filterValid_ :: F.SrcSpan -> F.Expr -> F.Cand a -> SmtM [a]
filterValid_ sp p qs = catMaybes <$> do
  smtAssertDecl p
  forM qs $ \(q, x) ->
    smtBracketAt sp "filterValidRHS" $ do
      smtAssertDecl (F.PNot q)
      valid <- smtCheckUnsat
      return $ if valid then Just x else Nothing

--------------------------------------------------------------------------------
-- | `filterValidGradual ps [(x1, q1),...,(xn, qn)]` returns the list `[ xi | p => qi]`
-- | for some p in the list ps
--------------------------------------------------------------------------------
filterValidGradual :: [F.Expr] -> F.Cand a -> SolveM ann [a]
--------------------------------------------------------------------------------
filterValidGradual p qs = do
  qs' <- liftSMT $
           smtBracket "filterValidGradualLHS" $
             filterValidGradual_ p qs
  -- stats
  incBrkt
  incChck (length qs)
  incVald (length qs')
  return qs'

filterValidGradual_ :: [F.Expr] -> F.Cand a -> SmtM [a]
filterValidGradual_ ps qs
  = map snd . fst <$> foldM partitionCandidates ([], qs) ps
  where
    partitionCandidates :: (F.Cand a, F.Cand a) -> F.Expr -> SmtM (F.Cand a, F.Cand a)
    partitionCandidates (ok, candidates) p = do
      (valids', invalids')  <- partition snd <$> filterValidOne_ p candidates
      let (valids, invalids) = (fst <$> valids', fst <$> invalids')
      return (ok ++ valids, invalids)

filterValidOne_ :: F.Expr -> F.Cand a -> SmtM [((F.Expr, a), Bool)]
filterValidOne_ p qs = do
  smtAssert p
  forM qs $ \(q, x) ->
    smtBracket "filterValidRHS" $ do
      smtAssert (F.PNot q)
      valid <- smtCheckUnsat
      return ((q, x), valid)

smtEnablembqi :: SolveM ann ()
smtEnablembqi
  = liftSMT smtSetMbqi

--------------------------------------------------------------------------------
checkSat :: F.Expr -> SolveM ann Bool
--------------------------------------------------------------------------------
checkSat p
  = liftSMT $
      smtBracket "checkSat" $
        smtCheckSat p

--------------------------------------------------------------------------------
assumesAxioms :: [F.Triggered F.Expr] -> SolveM ann ()
--------------------------------------------------------------------------------
assumesAxioms es = liftSMT $ forM_ es smtAssertAxiom


---------------------------------------------------------------------------
stats :: SolveM ann Stats
---------------------------------------------------------------------------
stats = ssStats <$> get

---------------------------------------------------------------------------
tickIter :: Bool -> SolveM ann Int
---------------------------------------------------------------------------
tickIter newScc = progIter newScc >> incIter >> getIter

progIter :: Bool -> SolveM ann ()
progIter newScc = lift $ when newScc progressTick
