{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

-- | Solve a system of horn-clause constraints ----------------------------

module Language.Fixpoint.Solver.Solve (solve) where

import           Control.Monad (filterM)
import           Control.Applicative ((<$>))
import qualified Data.HashMap.Strict  as M
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Config
import           Language.Fixpoint.Solver.Validate
import qualified Language.Fixpoint.Solver.Solution as S
import qualified Language.Fixpoint.Solver.Worklist as W
import           Language.Fixpoint.Solver.Monad

-- DEBUG
import           Text.Printf
import           Language.Fixpoint.PrettyPrint
import           Debug.Trace

---------------------------------------------------------------------------
solve :: Config -> F.FInfo a -> IO (F.Result a)
---------------------------------------------------------------------------
solve cfg fi'  = runSolverM cfg fi' $ solve_ cfg fi'
  -- where
  --   Right fi' = validate cfg fi

---------------------------------------------------------------------------
solve_ :: Config -> F.FInfo a -> SolveM (F.Result a)
---------------------------------------------------------------------------
solve_ cfg fi = refine s0 wkl >>= result fi
  where
    s0        = trace "DONE: S.init" $ S.init cfg fi
    wkl       = trace "DONE: W.init" $ W.init cfg fi

---------------------------------------------------------------------------
refine :: S.Solution -> W.Worklist a -> SolveM S.Solution
---------------------------------------------------------------------------
refine s w
  | Just (c, w') <- W.pop w = do i       <- tickIter
                                 (b, s') <- refineC i s c
                                 let w'' = if b then W.push c w' else w'
                                 refine s' $ trace (refineMsg i c b w'')
                                           $ w''
  | otherwise               = return s

-- DEBUG
refineMsg i c b w = printf "REFINE: iter = %d cid = %s change = %s wkl = %s"
                      i (show $ F.sid c) (show b) (showpp w)

---------------------------------------------------------------------------
-- | Single Step Refinement -----------------------------------------------
---------------------------------------------------------------------------
refineC :: Int -> S.Solution -> F.SubC a -> SolveM (Bool, S.Solution)
---------------------------------------------------------------------------
refineC _i s c
  | null rhs  = return (False, s)
  | otherwise = do lhs   <- lhsPred  s c <$> getBinds
                   kqs   <- filterValid lhs rhs
                   return $ S.update s ks {-  $ tracepp (msg ks rhs kqs) -} kqs
  where
    (ks, rhs) = rhsCands s c
    -- msg ks xs ys = printf "refineC: iter = %d, ks = %s, rhs = %d, rhs' = %d \n" _i (showpp ks) (length xs) (length ys)

lhsPred :: S.Solution -> F.SubC a -> F.BindEnv -> F.Pred
lhsPred s c be = F.pAnd $ pGrd : pLhs : pBinds
  where
    pGrd       = F.sgrd c
    pLhs       = S.apply s  $  F.lhsCs c
    pBinds     = S.apply s <$> xts
    xts        = F.envCs be $  F.senv c

rhsCands :: S.Solution -> F.SubC a -> ([F.KVar], S.Cand (F.KVar, S.EQual))
rhsCands s c   = (fst <$> ks, kqs)
  where
    kqs        = [ cnd k su q | (k, su) <- ks, q <- S.lookup s k]
    ks         = predKs . F.reftPred . F.rhsCs $ c
    cnd k su q = (F.subst su (S.eqPred q), (k, q))

predKs :: F.Pred -> [(F.KVar, F.Subst)]
predKs (F.PAnd ps)    = concatMap predKs ps
predKs (F.PKVar k su) = [(k, su)]
predKs _              = []

---------------------------------------------------------------------------
-- | Convert Solution into Result -----------------------------------------
---------------------------------------------------------------------------
result :: F.FInfo a -> S.Solution -> SolveM (F.Result a)
---------------------------------------------------------------------------
result fi s = do
  let sol  = M.map (F.pAnd . fmap S.eqPred) s
  stat    <- result_ fi s
  return   $ F.Result stat sol

result_ :: F.FInfo a -> S.Solution -> SolveM (F.FixResult (F.SubC a))
result_ fi s = res <$> filterM (isUnsat s) cs
  where
    cs       = M.elems $ F.cm fi
    res []   = F.Safe
    res cs'  = F.Unsafe cs'

---------------------------------------------------------------------------
isUnsat :: S.Solution -> F.SubC a -> SolveM Bool
---------------------------------------------------------------------------
isUnsat s c = do
  lp    <- lhsPred s c <$> getBinds
  let rp = rhsPred s c
  not   <$> isValid lp rp

isValid :: F.Pred -> F.Pred -> SolveM Bool
isValid p q = (not . null) <$> filterValid p [(q, ())]

rhsPred :: S.Solution -> F.SubC a -> F.Pred
rhsPred s c = S.apply s $ F.rhsCs c
