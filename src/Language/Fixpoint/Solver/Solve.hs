{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | Solve a system of horn-clause constraints ---------------------------------
--------------------------------------------------------------------------------

module Language.Fixpoint.Solver.Solve (solve, gradualSolve ) where

import           Control.Monad (when, filterM)
import           Control.Monad.State.Strict (lift)
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types           as F
import qualified Language.Fixpoint.Types.Solutions as Sol --F
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Config hiding (stats)
import qualified Language.Fixpoint.Solver.Solution  as S
import qualified Language.Fixpoint.Solver.Worklist  as W
import qualified Language.Fixpoint.Solver.Eliminate as E
import           Language.Fixpoint.Solver.Monad
import           Language.Fixpoint.Utils.Progress
import           Language.Fixpoint.Graph
import           Text.PrettyPrint.HughesPJ
import           Text.Printf
import           System.Console.CmdArgs.Verbosity (whenNormal, whenLoud)
import           Control.DeepSeq
import           Data.List  (sort)
import           Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- DEBUG
-- import           Debug.Trace (trace)

--------------------------------------------------------------------------------
solve :: (NFData a, F.Fixpoint a) => Config -> F.SInfo a -> IO (F.Result (Integer, a))
--------------------------------------------------------------------------------
solve cfg fi = do
    -- donePhase Loud "Worklist Initialize"
    (res, stat) <- withProgressFI sI $ runSolverM cfg sI n act
    when (solverStats cfg) $ printStats fi wkl stat
    -- print (numIter stat)
    return res
  where
    act  = solve_ cfg fi s0 ks  wkl
    sI   = solverInfo cfg fi
    wkl  = W.init sI
    n    = fromIntegral $ W.wRanks wkl
    s0   = siSol  sI
    ks   = siVars sI

--------------------------------------------------------------------------------
-- | Progress Bar
--------------------------------------------------------------------------------
withProgressFI :: SolverInfo a -> IO b -> IO b
withProgressFI = withProgress . fromIntegral . cNumScc . siDeps
--------------------------------------------------------------------------------

printStats :: F.SInfo a ->  W.Worklist a -> Stats -> IO ()
printStats fi w s = putStrLn "\n" >> ppTs [ ptable fi, ptable s, ptable w ]
  where
    ppTs          = putStrLn . showpp . mconcat

--------------------------------------------------------------------------------
solverInfo :: Config -> F.SInfo a -> SolverInfo a
--------------------------------------------------------------------------------
solverInfo cfg fI
  | useElim cfg = E.solverInfo cfg fI
  | otherwise   = SI mempty fI cD (siKvars fI)
  where
    cD          = elimDeps fI (kvEdges fI) mempty

siKvars :: F.SInfo a -> S.HashSet F.KVar
siKvars = S.fromList . M.keys . F.ws


--------------------------------------------------------------------------------
solve_ :: (NFData a, F.Fixpoint a)
       => Config
       -> F.SInfo a
       -> Sol.Solution
       -> S.HashSet F.KVar
       -> W.Worklist a
       -> SolveM (F.Result (Integer, a), Stats)
--------------------------------------------------------------------------------
solve_ cfg fi s0 ks wkl = do
  -- lift $ dumpSolution "solve_.s0" s0
  let s0'  = mappend s0 $ {-# SCC "sol-init" #-} S.init cfg fi ks
  s       <- {-# SCC "sol-refine" #-} refine s0' wkl
  res     <- {-# SCC "sol-result" #-} result cfg wkl s
  st      <- stats
  let res' = {-# SCC "sol-tidy"   #-} tidyResult res
  return $!! (res', st)

--------------------------------------------------------------------------------
-- | tidyResult ensures we replace the temporary kVarArg names introduced to
--   ensure uniqueness with the original names in the given WF constraints.
--------------------------------------------------------------------------------

tidyResult :: F.Result a -> F.Result a
tidyResult r = r { F.resSolution = tidySolution (F.resSolution r) }

tidySolution :: F.FixSolution -> F.FixSolution
tidySolution = fmap tidyPred

tidyPred :: F.Expr -> F.Expr
tidyPred = F.substf (F.eVar . F.tidySymbol)

--------------------------------------------------------------------------------
refine :: Sol.Solution -> W.Worklist a -> SolveM Sol.Solution
--------------------------------------------------------------------------------
refine s w
  | Just (c, w', newScc, rnk) <- W.pop w = do
     i       <- tickIter newScc
     (b, s') <- refineC i s c
     lift $ writeLoud $ refineMsg i c b rnk
     let w'' = if b then W.push c w' else w'
     refine s' w''
  | otherwise = return s
  where
    -- DEBUG
    refineMsg i c b rnk = printf "\niter=%d id=%d change=%s rank=%d\n"
                            i (F.subcId c) (show b) rnk

---------------------------------------------------------------------------
-- | Single Step Refinement -----------------------------------------------
---------------------------------------------------------------------------
refineC :: Int -> Sol.Solution -> F.SimpC a -> SolveM (Bool, Sol.Solution)
---------------------------------------------------------------------------
refineC _i s c
  | null rhs  = return (False, s)
  | otherwise = do be     <- getBinds
                   let lhs = {- F.tracepp ("LHS at " ++ show _i) $ -} S.lhsPred be s c
                   kqs    <- filterValid lhs rhs
                   return  $ S.update s ks {- tracepp (msg ks rhs kqs) -} kqs
  where
    _ci       = F.subcId c
    (ks, rhs) = rhsCands s c
    -- msg       = printf "refineC: iter = %d, sid = %s, soln = \n%s\n"
    --               _i (show (F.sid c)) (showpp s)
    _msg ks xs ys = printf "refineC: iter = %d, sid = %s, s = %s, rhs = %d, rhs' = %d \n"
                     _i (show _ci) (showpp ks) (length xs) (length ys)

rhsCands :: Sol.Solution -> F.SimpC a -> ([F.KVar], Sol.Cand (F.KVar, Sol.EQual))
rhsCands s c    = (fst <$> ks, kqs)
  where
    kqs         = [ (p, (k, q)) | (k, su) <- ks, (p, q)  <- cnd k su ]
    ks          = predKs . F.crhs $ c
    cnd k su    = Sol.qbPreds msg s su (Sol.lookupQBind s k)
    msg         = "rhsCands: " ++ show (F.sid c)

predKs :: F.Expr -> [(F.KVar, F.Subst)]
predKs (F.PAnd ps)    = concatMap predKs ps
predKs (F.PKVar k su) = [(k, su)]
predKs _              = []

--------------------------------------------------------------------------------
-- | Convert Solution into Result ----------------------------------------------
--------------------------------------------------------------------------------
result :: (F.Fixpoint a) => Config -> W.Worklist a -> Sol.Solution -> SolveM (F.Result (Integer, a))
--------------------------------------------------------------------------------
result _ wkl s = do
  lift $ writeLoud "Computing Result"
  stat    <- result_ wkl s
  -- stat'   <- gradualSolve cfg stat
  lift $ whenNormal $ putStrLn $ "RESULT: " ++ show (F.sid <$> stat)
  return   $  F.Result (ci <$> stat) (Sol.result s)
  where
    ci c = (F.subcId c, F.sinfo c)

result_ :: W.Worklist a -> Sol.Solution -> SolveM (F.FixResult (F.SimpC a))
result_  w s = res <$> filterM (isUnsat s) cs
  where
    cs       = W.unsatCandidates w
    res []   = F.Safe
    res cs'  = F.Unsafe cs'

---------------------------------------------------------------------------
isUnsat :: Sol.Solution -> F.SimpC a -> SolveM Bool
---------------------------------------------------------------------------
isUnsat s c = do
  -- lift   $ printf "isUnsat %s" (show (F.subcId c))
  _     <- tickIter True -- newScc
  be    <- getBinds
  let lp = S.lhsPred be s c
  let rp = rhsPred        c
  res   <- not <$> isValid lp rp
  lift   $ whenLoud $ showUnsat res (F.subcId c) lp rp
  return res

showUnsat :: Bool -> Integer -> F.Pred -> F.Pred -> IO ()
showUnsat u i lP rP = {- when u $ -} do
  putStrLn $ printf   "UNSAT id %s %s" (show i) (show u)
  putStrLn $ showpp $ "LHS:" <+> pprint lP
  putStrLn $ showpp $ "RHS:" <+> pprint rP

--------------------------------------------------------------------------------
-- | Predicate corresponding to RHS of constraint in current solution
--------------------------------------------------------------------------------
rhsPred :: F.SimpC a -> F.Expr
--------------------------------------------------------------------------------
rhsPred c
  | isTarget c = F.crhs c
  | otherwise  = errorstar $ "rhsPred on non-target: " ++ show (F.sid c)

isValid :: F.Expr -> F.Expr -> SolveM Bool
isValid p q = (not . null) <$> filterValid p [(q, ())]


--------------------------------------------------------------------------------
-- | RJ: @nikivazou please add some description here of what this does.
--------------------------------------------------------------------------------
gradualSolve :: (Fixpoint a)
             => Config
             -> F.FixResult (F.SimpC a)
             -> SolveM (F.FixResult (F.SimpC a))
gradualSolve cfg (F.Unsafe cs)
  | gradual cfg   = go cs
  where
    go cs         = smtEnablembqi >> (makeResult . catMaybes <$> mapM gradualSolveOne cs)
    makeResult    = applyNonNull F.Safe F.Unsafe
gradualSolve _  r = return r

gradualSolveOne :: (F.Fixpoint a) => F.SimpC a -> SolveM (Maybe (F.SimpC a))
gradualSolveOne c =
  do γ0 <- makeEnvironment c
     let (γ, γ', hasGradual) = splitLastGradual γ0
     if hasGradual
      then do let vc = makeGradualExpression γ γ' (F.crhs c)
              s <- checkSat vc
              return {- traceShow ("DEBUG" ++ show  (γ, γ', F.crhs c) ++ "\nVC = \n" ++ show (vc, s) ) -}
                 $ if s then Nothing else Just c
      else return $ Just c

makeGradualExpression :: [(F.Symbol, F.SortedReft)] -> [(F.Symbol, F.SortedReft)] -> F.Expr -> F.Expr
makeGradualExpression γ γ' p
  = F.PAnd [F.PAll bs (F.PImp gs p) [], gs]
  where
    bs = [ (x, s) | (x, F.RR s _) <- γ']
    gs = F.pAnd (bindToLogic <$> (γ ++ γ'))
    bindToLogic (x, F.RR _ (F.Reft (v, e))) = e `F.subst1` (v, F.EVar x)


-- makeEnvironment :: F.TaggedC c a => c a -> SolveM [(F.Symbol, F.SortedReft)]
makeEnvironment :: F.SimpC a -> SolveM [(F.Symbol, F.SortedReft)]
makeEnvironment c = cstrBinders c . F.soeBinds <$> getBinds

cstrBinders :: F.SimpC a -> F.BindEnv -> [(F.Symbol, F.SortedReft)]
cstrBinders c be = [ F.lookupBindEnv i be | i <- is  ]
  where
    is           = sort $ F.elemsIBindEnv $ F.senv c

splitLastGradual :: [(a, F.SortedReft)] -> ([(a, F.SortedReft)], [(a, F.SortedReft)], Bool)
splitLastGradual = go [] . reverse
  where
    go acc (xe@(x, F.RR s (F.Reft (v, e))) : xss)
      | Just es <- removePGrads e
      = (reverse ((x, F.RR s (F.Reft (v, F.pAnd es))):xss), reverse acc, True)
      | otherwise
      = go (xe:acc) xss
    go acc []
      = ([], reverse acc, False)

removePGrads :: F.Expr -> Maybe [F.Expr]
removePGrads (F.PAnd es)
  | F.PGrad `elem` es
  = Just $ filter (/= F.PGrad) es
  | otherwise
  = Nothing
removePGrads F.PGrad
  = Just []
removePGrads _
  = Nothing

{-
---------------------------------------------------------------------------
donePhase' :: String -> SolveM ()
---------------------------------------------------------------------------
donePhase' msg = lift $ do
  threadDelay 25000
  putBlankLn
  donePhase Loud msg
-}
