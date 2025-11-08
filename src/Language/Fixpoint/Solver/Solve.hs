{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

--------------------------------------------------------------------------------
-- | Solve a system of horn-clause constraints ---------------------------------
--------------------------------------------------------------------------------

module Language.Fixpoint.Solver.Solve (solve) where

import           Control.Monad (when, filterM)
import           Control.Monad.Reader
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Misc            as Misc
import qualified Language.Fixpoint.Types           as F
import qualified Language.Fixpoint.Types.Solutions as Sol
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Config hiding (stats)
import           Language.Fixpoint.SortCheck          (ElabM)
import qualified Language.Fixpoint.Solver.Solution  as S
import qualified Language.Fixpoint.Smt.Types as T
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
import Language.Fixpoint.Solver.Interpreter (instInterpreter)
import qualified Language.Fixpoint.Solver.PLE as PLE      (instantiate)
import Data.Maybe (maybeToList)
-- import Debug.Trace                      (trace)

mytrace :: String -> a -> a
mytrace
  -- s x = trace s x
  _ x = x
{-
solve_ :: (NFData a, F.Fixpoint a, F.Loc a)
       => Config
       -> F.SInfo a
       -> Sol.Solution
       -> W.Worklist a
       -> SolveM a (F.Result (Integer, a), Stats)
       -}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
solve :: forall a. (NFData a, F.Fixpoint a, Show a, F.Loc a) => Config -> F.SInfo a -> IO (F.Result (Integer, a))
--------------------------------------------------------------------------------

solve cfg fi = do
    whenLoud $ donePhase Misc.Loud "Worklist Initialize"
    vb <- getVerbosity
    (res, stat) <- (if Quiet == vb then id else withProgressFI sI) $ runSolverM cfg sI act
    when (solverStats cfg) $ printStats fi wkl stat
    -- print (numIter stat)
    return res
  where
    act :: SolveM a (F.Result (Integer, a), Stats)
    act = solve_ cfg fi s0 wkl
    -- solverInfo computes the set of cut and non-cut kvars, then initializes
    -- the solutions of the non-cut KVars (in the sHyp field)
    --
    -- S.init provides an initial solution for the cut KVars
    sI  = solverInfo cfg fi
    wkl = W.init sI
    s0  = mappend (siSol sI) (S.init cfg fi ks)
    ks  = siVars sI


--------------------------------------------------------------------------------
-- | Progress Bar
--------------------------------------------------------------------------------
withProgressFI :: SolverInfo a -> IO b -> IO b
withProgressFI = withProgress . (+ 1) . fromIntegral . cNumScc . siDeps
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
    cD          = elimDeps fI (kvEdges fI) mempty mempty

siKvars :: F.SInfo a -> S.HashSet F.KVar
siKvars = S.fromList . M.keys . F.ws

doInterpret :: (F.Loc a) =>  Config -> F.SInfo a -> [F.SubcId] -> SolveM a (F.BindEnv a)
doInterpret cfg fi subcIds = liftIO $ instInterpreter cfg fi (Just subcIds)

--------------------------------------------------------------------------------
{-# SCC solve_ #-}
solve_ :: (NFData a, F.Fixpoint a, F.Loc a)
       => Config
       -> F.SInfo a
       -> Sol.Solution
       -> W.Worklist a
       -> SolveM a (F.Result (Integer, a), Stats)
--------------------------------------------------------------------------------
solve_ cfg fi s2 wkl = do
  (s3, res0) <- sendConcreteBindingsToSMT F.emptyIBindEnv (F.bs fi) $ \bindingsInSmt -> do
    -- let s3   = solveEbinds fi s2
    s3       <- {- SCC "sol-refine" -} refine bindingsInSmt (F.bs fi) s2 wkl
    res0     <- {- SCC "sol-result" -} result bindingsInSmt cfg fi (W.unsatCandidates wkl) s3
    return (s3, res0)

  (fi1, res1) <- case resStatus res0 of  {- first run the interpreter -}
    Unsafe _ bads | not (noLazyPLE cfg) && rewriteAxioms cfg && interpreter cfg -> do
      bs <- doInterpret cfg fi (map fst $ mytrace ("before the Interpreter " ++ show (length bads) ++ " constraints remain") bads)
      -- TODO the `clearApplys` is a workaround needed because `sendConcreteBindingsToSMT`
      -- seems to not remove the tags introduced in its bracket from the tag stack,
      -- meanwhile the SMT solver pops the corresponding definition. The result is
      -- that when the same definition needs to re-emitted in the interpreter/PLE,
      -- LH thinks it's still in the context, which causes the SMT solver to crash.
      clearApplys
      let fi1 = fi { F.bs = bs }
          badCs = lookupCMap (F.cm fi) <$> map fst bads
      fmap (fi1,) $ sendConcreteBindingsToSMT F.emptyIBindEnv bs $ \bindingsInSmt ->
        result bindingsInSmt cfg fi1 badCs s3
    _ -> return  (fi, mytrace "all checked before interpreter" res0)

  res2  <- case resStatus res1 of  {- then run normal PLE on remaining unsolved constraints -}
    Unsafe _ bads2 | not (noLazyPLE cfg) && rewriteAxioms cfg -> do
      bs <- liftIO $ PLE.instantiate cfg fi1 (Just s3) (Just $ map fst bads2)
      -- TODO reset the ix stack too?
      clearApplys
      -- Check the constraints one last time after PLE
      let fi2 = fi { F.bs = bs }
          badsCs2 = lookupCMap (F.cm fi) <$> map fst bads2
      sendConcreteBindingsToSMT F.emptyIBindEnv bs $ \bindingsInSmt ->
        result bindingsInSmt cfg fi2 badsCs2 s3
    _ -> return $ mytrace "all checked with interpreter" res1

  st      <- stats
  let res3 = {- SCC "sol-tidy" -} tidyResult cfg res2
  return $!! (res3, st)


--------------------------------------------------------------------------------
-- | tidyResult ensures we replace the temporary kVarArg names introduced to
--   ensure uniqueness with the original names in the given WF constraints.
--------------------------------------------------------------------------------
tidyResult :: Config -> F.Result a -> F.Result a
tidyResult _ r = r
  { F.resSolution = tidySolution (F.resSolution r)
  , F.resNonCutsSolution = tidySolution (F.resNonCutsSolution r)
  , F.resSorts = fmap tidyBind <$>  F.resSorts r
  }

tidySolution :: F.FixSolution -> F.FixSolution
tidySolution = fmap tidyPred

tidyBind :: (F.Symbol, F.Sort) -> (F.Symbol, F.Sort)
tidyBind (x, t) = (F.tidySymbol x, t)

tidyPred :: F.Expr -> F.Expr
tidyPred =  go
  where
    ts = F.tidySymbol
    tb = tidyBind
    go (F.EApp s e)      = F.EApp (go s) (go e)
    go (F.ELam (x,t) e)  = F.ELam (ts x, t) (go e)
    go (F.ECoerc a t e)  = F.ECoerc a t (go e)
    go (F.ENeg e)        = F.ENeg (go e)
    go (F.EBin op e1 e2) = F.EBin op (go e1) (go e2)
    go (F.ELet x e1 e2)  = F.ELet (ts x) (go e1) (go e2)
    go (F.EIte p e1 e2)  = F.EIte (go p) (go e1) (go e2)
    go (F.ECst e so)     = F.ECst (go e) so
    go (F.EVar x)        = F.EVar (ts x)
    go (F.PAnd ps)       = F.PAnd $ map go ps
    go (F.POr  ps)       = F.POr  $ map go ps
    go (F.PNot p)        = F.PNot $ go p
    go (F.PImp p1 p2)    = F.PImp (go p1) (go p2)
    go (F.PIff p1 p2)    = F.PIff (go p1) (go p2)
    go (F.PAtom r e1 e2) = F.PAtom r (go e1) (go e2)
    go (F.PExist xts e)  = F.PExist (tb <$> xts) (go e)
    go (F.PAll xts e)    = F.PAll   (tb <$> xts) (go e)
    go  p                = p

--------------------------------------------------------------------------------
{-# SCC refine #-}
-- | Implementation of the inference algorithm from:
--
-- "Liquid Types", PLDI 2008, https://ranjitjhala.github.io/static/liquid_types.pdf
--
refine
  :: (F.Loc a)
  => F.IBindEnv
  -> F.BindEnv a
  -> Sol.Solution
  -> W.Worklist a
  -> SolveM a Sol.Solution
--------------------------------------------------------------------------------
refine bindingsInSmt be s w
  | Just (c, w', newScc, rnk) <- W.pop w = do
     i       <- tickIter newScc
     (b, s') <- refineC bindingsInSmt be i s c
     lift $ writeLoud $ refineMsg i c b rnk (showpp s')
     let w'' = if b then W.push c w' else w'
     refine bindingsInSmt be s' w''
  | otherwise = return s
  where
    -- DEBUG
    refineMsg i c b rnk s = printf "\niter=%d id=%d change=%s rank=%d s=%s\n"
                             i (F.subcId c) (show b) rnk s

---------------------------------------------------------------------------
-- | Single Step Refinement -----------------------------------------------
---------------------------------------------------------------------------
{-# SCC refineC #-}
refineC
  :: (F.Loc a)
  => F.IBindEnv
  -> F.BindEnv a
  -> Int
  -> Sol.Solution
  -> F.SimpC a
  -> SolveM a (Bool, Sol.Solution)
---------------------------------------------------------------------------
refineC bindingsInSmt be _i s c =
  do ef <- T.ctxElabF <$> getContext
     let (ks, rhs) = runReader (rhsCands s c) ef
     if null rhs
        then return (False, s)
        else do let lhs = runReader (S.lhsPred bindingsInSmt (F.coerceBindEnv ef be) s c) ef
                kqs    <- filterValid (cstrSpan c) lhs rhs
                return  $ S.update s ks kqs
  where
    _ci       = F.subcId c
    -- msg       = printf "refineC: iter = %d, sid = %s, soln = \n%s\n"
    --               _i (show (F.sid c)) (showpp s)
    _msg ks xs ys = printf "refineC: iter = %d, sid = %s, s = %s, rhs = %d, rhs' = %d \n"
                     _i (show _ci) (showpp ks) (length xs) (length ys)

rhsCands :: Sol.Solution -> F.SimpC a -> ElabM ([F.KVar], Sol.Cand (F.KVar, Sol.EQual))
rhsCands s c    =
  do pq <- traverse cnd ks
     pure (fst <$> ks, concat pq)
  where
    cnd :: (F.KVar, F.Subst) -> ElabM [(F.Pred, (F.KVar, Sol.EQual))]
    cnd (k, su) = map (\(p , q) -> (p , (k , q))) <$> Sol.qbPreds msg s su (Sol.lookupQBind s k)
    ks          = predKs . F.crhs $ c

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
  -> F.SInfo a
  -> [F.SimpC a]
  -> Sol.Solution
  -> SolveM a (F.Result (Integer, a))
--------------------------------------------------------------------------------
result bindingsInSmt cfg fi cs s =
  sendConcreteBindingsToSMT bindingsInSmt be $ \bindingsInSmt2 -> do
    lift       $ writeLoud "Computing Result"
    stat      <- result_ bindingsInSmt2 be cfg cs s
    lift       $ whenLoud $ putStrLn $ "RESULT: " ++ show (F.sid <$> stat)
    resCut    <- solResult cfg s
    resNonCut <- solNonCutsResult cfg be s
    let resSorts = resultSorts fi (M.keys resCut ++ M.keys resNonCut) be
    return     $ F.Result (ci <$> stat) resCut resNonCut resSorts
  where
    ci c = (F.subcId c, F.sinfo c)
    be = F.bs fi

resultSorts :: F.SInfo a -> [F.KVar] -> F.BindEnv a -> F.ResultSorts
resultSorts fi ks be = M.fromList
  [(k, xts)
    | k <- ks
    , xts <- maybeToList (kvarScope fi be k) ]

kvarScope :: F.SInfo a -> F.BindEnv a -> F.KVar -> Maybe [(F.Symbol, F.Sort)]
kvarScope fi be k = do
  w <- M.lookup k (F.ws fi)
  let bs = F.wenv w
  let (v, t, _) = F.wrft w
  return $ (v, t) : [ bindInfo be i | i <- L.sort (F.elemsIBindEnv bs) ]

bindInfo :: F.BindEnv a -> F.BindId -> (F.Symbol, F.Sort)
bindInfo be i = (x, F.sr_sort sr)
  where
    (x, sr, _) = F.lookupBindEnv i be

solResult :: Config -> Sol.Solution -> SolveM ann (M.HashMap F.KVar F.Expr)
solResult cfg = minimizeResult cfg . Sol.result

solNonCutsResult :: Config -> F.BindEnv a -> Sol.Solution -> SolveM ann (M.HashMap F.KVar F.Expr)
solNonCutsResult cfg be s
  | cfgNonCuts cfg = do
    ef <- T.ctxElabF <$> getContext
    pure $ runReader (S.nonCutsResult be s) ef
  | otherwise = pure mempty

cfgNonCuts :: Config -> Bool
cfgNonCuts cfg = save cfg || json cfg

result_
  :: (F.Loc a, NFData a)
  => F.IBindEnv
  -> F.BindEnv a
  -> Config
  -> [F.SimpC a]
  -> Sol.Solution
  -> SolveM a (F.FixResult (F.SimpC a))
result_ bindingsInSmt be cfg cs0 s = do
  filtered <- filterM (isUnsat bindingsInSmt be s) cs
  sts      <- stats
  pure $ res sts filtered
  where
    cs          = isChecked cfg cs0
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
  :: (F.Loc a, NFData a) => F.IBindEnv -> F.BindEnv a -> Sol.Solution -> F.SimpC a -> SolveM a Bool
--------------------------------------------------------------------------------
isUnsat bindingsInSmt be s c = do
  -- lift   $ printf "isUnsat %s" (show (F.subcId c))
  _     <- tickIter True -- newScc
  ef <- T.ctxElabF <$> getContext
  let lp = runReader (S.lhsPred bindingsInSmt (F.coerceBindEnv ef be) s c) ef
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
