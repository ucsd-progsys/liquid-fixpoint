{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

--------------------------------------------------------------------------------
-- | Solve a system of horn-clause constraints ---------------------------------
--------------------------------------------------------------------------------

module Language.Fixpoint.Solver.Solve (solve, solverInfo) where

import           Control.Monad (when, filterM)
import           Control.Monad.State.Strict (liftIO, modify, lift)
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Misc            as Misc
import qualified Language.Fixpoint.Types           as F
import qualified Language.Fixpoint.Types.Solutions as Sol
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
import           System.Console.CmdArgs.Verbosity -- (whenNormal, whenLoud)
import           Control.DeepSeq
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
-- import qualified Data.Maybe          as Mb
import qualified Data.List           as L
import Language.Fixpoint.Types (resStatus, FixResult(Unsafe))
import qualified Language.Fixpoint.Types.Config as C
import Language.Fixpoint.Solver.Interpreter (instInterpreter)
import Language.Fixpoint.Solver.Instantiate (instantiate)
--import Debug.Trace                      (trace)

mytrace :: String -> a -> a
mytrace _ x = {- trace -} x

--------------------------------------------------------------------------------
solve :: (NFData a, F.Fixpoint a, Show a, F.Loc a) => Config -> F.SInfo a -> IO (F.Result (Integer, a))
--------------------------------------------------------------------------------

solve cfg fi = do
    whenLoud $ donePhase Misc.Loud "Worklist Initialize"
    vb <- getVerbosity
    (res, stat) <- (if Quiet == vb || gradual cfg then id else withProgressFI sI) $ runSolverM cfg sI act
    when (solverStats cfg) $ printStats fi wkl stat
    -- print (numIter stat)
    return res
  where
    act  = solve_ cfg fi s0 ks  wkl
    sI   = solverInfo cfg fi
    wkl  = W.init sI
    s0   = siSol  sI
    ks   = siVars sI


--------------------------------------------------------------------------------
-- | Progress Bar
--------------------------------------------------------------------------------
withProgressFI :: SolverInfo a b -> IO b -> IO b
withProgressFI = withProgress . (+ 1) . fromIntegral . cNumScc . siDeps
--------------------------------------------------------------------------------

printStats :: F.SInfo a ->  W.Worklist a -> Stats -> IO ()
printStats fi w s = putStrLn "\n" >> ppTs [ ptable fi, ptable s, ptable w ]
  where
    ppTs          = putStrLn . showpp . mconcat

--------------------------------------------------------------------------------
solverInfo :: Config -> F.SInfo a -> SolverInfo a b
--------------------------------------------------------------------------------
solverInfo cfg fI
  | useElim cfg = E.solverInfo cfg fI
  | otherwise   = SI mempty fI cD (siKvars fI)
  where
    cD          = elimDeps fI (kvEdges fI) mempty mempty

siKvars :: F.SInfo a -> S.HashSet F.KVar
siKvars = S.fromList . M.keys . F.ws

doInterpret :: (F.Loc a) =>  Config -> F.SInfo a -> [F.SubcId] -> SolveM a (F.SInfo a)
doInterpret cfg fi0 subcIds = do
  fi <- liftIO $ instInterpreter cfg fi0 (Just subcIds)
  modify $ update' fi
  return fi
  where
    update' fi ss = ss{ssBinds = F.bs fi'}
      where
        fi' = (siQuery sI) {F.hoInfo = F.HOI (C.allowHO cfg) (C.allowHOqs cfg)}
        sI  = solverInfo cfg fi

{-# SCC doPLE #-}
doPLE :: (F.Loc a) =>  Config -> F.SInfo a -> [F.SubcId] -> SolveM a ()
doPLE cfg fi0 subcIds = do
  fi <- liftIO $ instantiate cfg fi0 (Just subcIds)
  modify $ update' fi
  where
    update' fi ss = ss{ssBinds = F.bs fi'}
      where
        fi' = (siQuery sI) {F.hoInfo = F.HOI (C.allowHO cfg) (C.allowHOqs cfg)}
        sI  = solverInfo cfg fi

--------------------------------------------------------------------------------
{-# SCC solve_ #-}
solve_ :: (NFData a, F.Fixpoint a, F.Loc a)
       => Config
       -> F.SInfo a
       -> Sol.Solution
       -> S.HashSet F.KVar
       -> W.Worklist a
       -> SolveM a (F.Result (Integer, a), Stats)
--------------------------------------------------------------------------------
solve_ cfg fi s0 ks wkl = do
  let s1   = {-# SCC "sol-init" #-} S.init cfg fi ks
  let s2   = mappend s0 s1
  (s3, res0) <- sendConcreteBindingsToSMT F.emptyIBindEnv $ \bindingsInSmt -> do
    -- let s3   = solveEbinds fi s2
    s3       <- {- SCC "sol-refine" -} refine bindingsInSmt s2 wkl
    res0     <- {- SCC "sol-result" -} result bindingsInSmt cfg wkl s3
    return (s3, res0)

  (fi1, s4, res1) <- case resStatus res0 of  {- first run the interpreter -}
    Unsafe _ bads | not (noLazyPLE cfg) && rewriteAxioms cfg && interpreter cfg -> do
      fi1 <- doInterpret cfg fi (map fst $ mytrace ("before the Interpreter " ++ show (length bads) ++ " constraints remain") bads)
      (s4, res1) <-  sendConcreteBindingsToSMT F.emptyIBindEnv $ \bindingsInSmt -> do
        s4    <- {- SCC "sol-refine" -} refine bindingsInSmt s3 wkl
        res1  <- {- SCC "sol-result" -} result bindingsInSmt cfg wkl s4
        return (s4, res1)
      return (fi1, s4, res1)
    _ -> return  (fi, s3, mytrace "all checked before interpreter" res0)

  res2  <- case resStatus res1 of  {- then run normal PLE on remaining unsolved constraints -}
    Unsafe _ bads2 | not (noLazyPLE cfg) && rewriteAxioms cfg -> do
      doPLE cfg fi1 (map fst $ mytrace ("before z3 PLE " ++ show (length bads2) ++ " constraints remain") bads2)
      sendConcreteBindingsToSMT F.emptyIBindEnv $ \bindingsInSmt -> do
        s5    <- {- SCC "sol-refine" -} refine bindingsInSmt s4 wkl
        result bindingsInSmt cfg wkl s5
    _ -> return $ mytrace "all checked with interpreter" res1

  st      <- stats
  let res3 = {- SCC "sol-tidy" -} tidyResult res2
  return $!! (res3, st)


--------------------------------------------------------------------------------
-- | tidyResult ensures we replace the temporary kVarArg names introduced to
--   ensure uniqueness with the original names in the given WF constraints.
--------------------------------------------------------------------------------
tidyResult :: F.Result a -> F.Result a
tidyResult r = r
  { F.resSolution = tidySolution (F.resSolution r)
  , F.resNonCutsSolution = tidySolution (F.resNonCutsSolution r)
  }

tidySolution :: F.FixSolution -> F.FixSolution
tidySolution = fmap tidyPred

tidyPred :: F.Expr -> F.Expr
tidyPred = F.substf (F.eVar . F.tidySymbol)

--------------------------------------------------------------------------------
{-# SCC refine #-}
refine
  :: (F.Loc a)
  => F.IBindEnv
  -> Sol.Solution
  -> W.Worklist a
  -> SolveM a Sol.Solution
--------------------------------------------------------------------------------
refine bindingsInSmt s w
  | Just (c, w', newScc, rnk) <- W.pop w = do
     i       <- tickIter newScc
     (b, s') <- refineC bindingsInSmt i s c
     lift $ writeLoud $ refineMsg i c b rnk
     let w'' = if b then W.push c w' else w'
     refine bindingsInSmt s' w''
  | otherwise = return s
  where
    -- DEBUG
    refineMsg i c b rnk = printf "\niter=%d id=%d change=%s rank=%d\n"
                            i (F.subcId c) (show b) rnk

---------------------------------------------------------------------------
-- | Single Step Refinement -----------------------------------------------
---------------------------------------------------------------------------
{-# SCC refineC #-}
refineC
  :: (F.Loc a)
  => F.IBindEnv
  -> Int
  -> Sol.Solution
  -> F.SimpC a
  -> SolveM a (Bool, Sol.Solution)
---------------------------------------------------------------------------
refineC bindingsInSmt _i s c
  | null rhs  = return (False, s)
  | otherwise = do be     <- getBinds
                   let lhs = S.lhsPred bindingsInSmt be s c
                   kqs    <- filterValid (cstrSpan c) lhs rhs
                   return  $ S.update s ks kqs
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
{-# SCC result #-}
result
  :: (F.Fixpoint a, F.Loc a, NFData a)
  => F.IBindEnv
  -> Config
  -> W.Worklist a
  -> Sol.Solution
  -> SolveM a (F.Result (Integer, a))
--------------------------------------------------------------------------------
result bindingsInSmt cfg wkl s =
  sendConcreteBindingsToSMT bindingsInSmt $ \bindingsInSmt2 -> do
    lift $ writeLoud "Computing Result"
    stat    <- result_ bindingsInSmt2 cfg wkl s
    lift $ whenLoud $ putStrLn $ "RESULT: " ++ show (F.sid <$> stat)

    F.Result (ci <$> stat) <$> solResult cfg s <*> solNonCutsResult s <*> return mempty
  where
    ci c = (F.subcId c, F.sinfo c)

solResult :: Config -> Sol.Solution -> SolveM ann (M.HashMap F.KVar F.Expr)
solResult cfg = minimizeResult cfg . Sol.result

solNonCutsResult :: Sol.Solution -> SolveM ann (M.HashMap F.KVar F.Expr)
solNonCutsResult s = do
  be <- getBinds
  return $ S.nonCutsResult be s

result_
  :: (F.Loc a, NFData a)
  => F.IBindEnv
  -> Config
  -> W.Worklist a
  -> Sol.Solution
  -> SolveM a (F.FixResult (F.SimpC a))
result_ bindingsInSmt cfg w s = do
  filtered <- filterM (isUnsat bindingsInSmt s) cs
  sts      <- stats
  pure $ res sts filtered
  where
    cs          = isChecked cfg (W.unsatCandidates w)
    res sts []  = F.Safe sts
    res sts cs' = F.Unsafe sts cs'

isChecked :: Config -> [F.SimpC a] -> [F.SimpC a]
isChecked cfg cs = case checkCstr cfg of
  []   -> cs
  ids  -> let s = S.fromList ids in
          [c | c <- cs, S.member (F.subcId c) s ]

--------------------------------------------------------------------------------
-- | `minimizeResult` transforms each KVar's result by removing
--   conjuncts that are implied by others. That is,
--
--      minimizeConjuncts :: ps:[Pred] -> {qs:[Pred] | subset qs ps}
--
--   such that `minimizeConjuncts ps` is a minimal subset of ps where no
--   is implied by /\_{q' in qs \ qs}
--   see: tests/pos/min00.fq for an example.
--------------------------------------------------------------------------------
minimizeResult :: Config -> M.HashMap F.KVar F.Expr
               -> SolveM ann (M.HashMap F.KVar F.Expr)
--------------------------------------------------------------------------------
minimizeResult cfg s
  | minimalSol cfg = mapM minimizeConjuncts s
  | otherwise      = return s

minimizeConjuncts :: F.Expr -> SolveM ann F.Expr
minimizeConjuncts p = F.pAnd <$> go (F.conjuncts p) []
  where
    go []     acc   = return acc
    go (p:ps) acc   = do b <- isValid F.dummySpan (F.pAnd (acc ++ ps)) p
                         if b then go ps acc
                              else go ps (p:acc)

--------------------------------------------------------------------------------
isUnsat
  :: (F.Loc a, NFData a) => F.IBindEnv -> Sol.Solution -> F.SimpC a -> SolveM a Bool
--------------------------------------------------------------------------------
isUnsat bindingsInSmt s c = do
  -- lift   $ printf "isUnsat %s" (show (F.subcId c))
  _     <- tickIter True -- newScc
  be    <- getBinds
  let lp = S.lhsPred bindingsInSmt be s c
  let rp = rhsPred        c
  res   <- not <$> isValid (cstrSpan c) lp rp
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

--------------------------------------------------------------------------------
isValid :: F.SrcSpan -> F.Expr -> F.Expr -> SolveM ann Bool
--------------------------------------------------------------------------------
isValid sp p q = not . null <$> filterValid sp p [(q, ())]

cstrSpan :: (F.Loc a) => F.SimpC a -> F.SrcSpan
cstrSpan = F.srcSpan . F.sinfo

{-
---------------------------------------------------------------------------
donePhase' :: String -> SolveM ()
---------------------------------------------------------------------------
donePhase' msg = lift $ do
  threadDelay 25000
  putBlankLn
  donePhase Loud msg
-}


-- NV TODO Move to a new file
-------------------------------------------------------------------------------
-- | Interaction with the user when Solving -----------------------------------
-------------------------------------------------------------------------------

_iMergePartitions :: [(Int, F.SInfo a)] -> IO [(Int, F.SInfo a)]
_iMergePartitions ifis = do
  putStrLn "Current Partitions are: "
  putStrLn $ unlines (partitionInfo <$> ifis)
  putStrLn "Merge Partitions? Y/N"
  c <- getChar
  if c == 'N'
    then do putStrLn "Solving Partitions"
            return ifis
    else do
      (i, j) <- getMergePartition (length ifis)
      _iMergePartitions (mergePartitions i j ifis)

getMergePartition :: Int -> IO (Int, Int)
getMergePartition n = do
  putStrLn "Which two partition to merge? (i, j)"
  ic <- getLine
  let (i,j) = read ic :: (Int, Int)
  if i < 1 || n < i || j < 1 || n < j
    then do putStrLn ("Invalid Partition numbers, write (i,j) with 1 <= i <= " ++ show n)
            getMergePartition n
    else return (i,j)

mergePartitions :: Int -> Int -> [(Int, F.SInfo a)] -> [(Int, F.SInfo a)]
mergePartitions i j fis
  = zip [1..] ((takei i `mappend` (takei j){F.bs = mempty}):rest)
  where
    takei i = snd (fis L.!! (i - 1))
    rest = snd <$> filter (\(k,_) -> k /= i && k /= j) fis

partitionInfo :: (Int, F.SInfo a) -> String
partitionInfo (i, fi)
  = "Partition number " ++ show i ++ "\n" ++
    "Defined ?? " ++ show defs    ++ "\n" ++
    "Used ?? "    ++ show uses
  where
    gs   = F.wloc . snd <$> L.filter (F.isGWfc . snd) (M.toList (F.ws fi))
    defs = L.nub (F.gsrc <$> gs)
    uses = L.nub (F.gused <$> gs)
