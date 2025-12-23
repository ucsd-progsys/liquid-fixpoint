--------------------------------------------------------------------------------
-- | This module implements "Proof by Logical Evaluation" where we
--   unfold function definitions if they *must* be unfolded, to strengthen
--   the environments with function-definition-equalities.
--   The algorithm is discussed at length in:
--
--     1. "Refinement Reflection", POPL 2018, https://arxiv.org/pdf/1711.03842
--     2. "Reasoning about Functions", VMCAI 2018, https://ranjitjhala.github.io/static/reasoning-about-functions.pdf
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DoAndIfThenElse           #-}

module Language.Fixpoint.Solver.PLE
  ( instantiate

  -- The following exports are for property testing.
  , FuelCount(..)
  , ICtx(..)
  , Knowledge(..)
  , simplify
  )
  where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Types.Config  as FC
import           Language.Fixpoint.Types.Solutions (CMap, Solution)
import qualified Language.Fixpoint.Types.Visitor as Vis
import qualified Language.Fixpoint.Misc          as Misc
import qualified Language.Fixpoint.Smt.Interface as SMT
import           Language.Fixpoint.Smt.Types (SmtM)
import           Language.Fixpoint.Defunctionalize
import           Language.Fixpoint.Solver.EnvironmentReduction (inlineInExpr, undoANF)
import qualified Language.Fixpoint.Utils.Files   as Files
import qualified Language.Fixpoint.Utils.Trie    as T
import           Language.Fixpoint.Utils.Progress
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Graph.Deps             (isTarget)
import           Language.Fixpoint.Solver.Common          (askSMT, toSMT)
import           Language.Fixpoint.Solver.Sanitize        (symbolEnv)
import           Language.Fixpoint.Solver.Simplify
import           Language.Fixpoint.Solver.Solution (CombinedEnv(..), applyInSortedReft)
import           Language.Fixpoint.Solver.Rewrite as Rewrite

import Language.REST.OCAlgebra as OC
import Language.REST.ExploredTerms as ExploredTerms
import Language.REST.RuntimeTerm as RT
import Language.REST.SMT (withZ3, SolverHandle)

import           Control.Monad (filterM, foldM, forM_, when, replicateM, zipWithM)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor (second)
import qualified Data.HashMap.Strict  as M
import qualified Data.HashMap.Lazy  as HashMap.Lazy
import qualified Data.HashSet         as S
import           Data.IORef
import qualified Data.List            as L
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe           as Mb
import qualified Data.Set as Set
import           Text.PrettyPrint.HughesPJ.Compat

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

--------------------------------------------------------------------------------
-- | Strengthen Constraint Environments via PLE
--
-- @instantiate cfg fi subcIds@ yields @F.bs fi@ strengthened with the
-- unfoldings discovered by PLE on the constraints in @subcIds@ (or all
-- constraints if @subcIds == Nothing@).
{-# SCC instantiate #-}
instantiate :: (Loc a) => Config -> SInfo a -> Maybe Solution -> Maybe [SubcId] -> SmtM (BindEnv a)
instantiate cfg fi' mSol subcIds = do
    let cs = M.filterWithKey
               (\i c -> isPleCstr aEnv i c && maybe True (i `L.elem`) subcIds)
               (cm info)
    let t  = mkCTrie (M.toList cs)                                          -- 1. BUILD the Trie
    res   <- withRESTSolver $ \solver -> do
               ctx <- get
               (res, ctx') <- liftIO $ withProgressM (`runStateT` ctx) (1 + M.size cs) $ do
                 env <- instEnv cfg info mSol cs solver
                 pleTrie t env                                              -- 2. TRAVERSE Trie to compute InstRes
               put ctx'
               return res
    liftIO $ savePLEEqualities cfg info sEnv res
    return $ resSInfo cfg sEnv info res                                     -- 3. STRENGTHEN SInfo using InstRes
  where
    withRESTSolver :: (Maybe SolverHandle -> SmtM a) -> SmtM a
    withRESTSolver f | all null (M.elems $ aenvAutoRW aEnv) = f Nothing
    withRESTSolver f = withZ3 (f . Just)

    sEnv = symbolEnv cfg info
    aEnv = ae info
    info = normalize fi'

savePLEEqualities :: Config -> SInfo a -> SymEnv -> InstRes -> IO ()
savePLEEqualities cfg info sEnv res = when (save cfg) $ do
    let fq   = queryFile Files.Fq cfg ++ ".ple"
    putStrLn $ "\nSaving PLE equalities: "   ++ fq ++ "\n"
    Misc.ensurePath fq
    let constraint_equalities =
          map equalitiesPerConstraint $ Misc.hashMapToAscList $ cm info
    writeFile fq $ render $ vcat $
      map renderConstraintRewrite constraint_equalities
  where
    equalitiesPerConstraint (cid, c) =
      (cid, L.sort [ e | i <- elemsIBindEnv (senv c), Just e <- [M.lookup i res] ])
    elabParam = ElabParam (solverFlags cfg) "savePLEEqualities" sEnv
    renderConstraintRewrite (cid, eqs) =
      "constraint id" <+> text (show cid ++ ":")
      $+$ nest 2
           (vcat $ L.intersperse "" $
            map (toFix . unElab) $ Set.toList $ Set.fromList $
            -- call elabExpr to try to bring equations that are missing
            -- some casts into a fully annotated form for comparison
            map (elabExpr elabParam (Just boolSort)) $
            concatMap conjuncts eqs
           )
      $+$ ""

-------------------------------------------------------------------------------
-- | Step 1a: @instEnv@ sets up the incremental-PLE environment
instEnv
  :: Loc a
  => Config
  -> SInfo a
  -> Maybe Solution
  -> CMap (SimpC a)
  -> Maybe SolverHandle
  -> SmtM (InstEnv a)
instEnv cfg info s cs restSolver = do
    ctx <- get
    refRESTCache <- liftIO $ newIORef mempty
    refRESTSatCache <- liftIO $ newIORef mempty
    let
        restOrd = FC.restOC cfg
        oc0 = ordConstraints restOrd $ Mb.fromJust restSolver
        oc :: OCAlgebra OCType RuntimeTerm IO
        oc = oc0
             { OC.isSat = cachedIsSat refRESTSatCache oc0
             , OC.notStrongerThan = cachedNotStrongerThan refRESTCache oc0
             }
        et :: ExploredTerms RuntimeTerm OCType IO
        et = ExploredTerms.empty
               EF
                 { ExploredTerms.union = OC.union oc
                 , ExploredTerms.subsumes = OC.notStrongerThan oc
                 , exRefine = OC.refine oc
                 }
                 ExploreWhenNeeded
        s0 = EvalEnv
              { evEnv = SMT.ctxSymEnv ctx
              , evElabF = ef
              , evKCtx = ctx
              , evExScope = []
              , evPendingUnfoldings = mempty
              , evNewEqualities = mempty
              , evSMTCache = mempty
              , evFuel = defFuelCount cfg
              , freshEtaNames = 0
              , explored = Just et
              , restSolver = restSolver
              , restOCA = restOrd
              , evOCAlgebra = oc
              }
    return $ InstEnv
       { ieCfg = cfg
       , ieBEnv = bs info
       , ieAenv = ae info
       , ieCstrs = cs
       , ieKnowl = knowledge cfg info
       , ieEvEnv = s0
       , ieLRWs  = lrws info
       , ieSol  = s
       }
  where
    ef = solverFlags cfg

    cachedNotStrongerThan refRESTCache oc a b = do
      m <- readIORef refRESTCache
      case M.lookup (a, b) m of
        Nothing -> do
          nst <- OC.notStrongerThan oc a b
          writeIORef refRESTCache (M.insert (a, b) nst m)
          return nst
        Just nst ->
          return nst

    cachedIsSat refRESTSatCache oc a = do
      m <- readIORef refRESTSatCache
      case M.lookup a m of
        Nothing -> do
          sat <- OC.isSat oc a
          writeIORef refRESTSatCache (M.insert a sat m)
          return sat
        Just sat ->
          return sat

----------------------------------------------------------------------------------------------
-- | Step 1b: @mkCTrie@ builds the @Trie@ of constraints indexed by their environments
--
-- The trie is a way to unfold the equalities a minimum number of times.
-- Say you have
--
-- > 1: [1, 2, 3, 4, 5] => p1
-- > 2: [1, 2, 3, 6, 7] => p2
--
-- Then you build the tree
--
-- >  1 -> 2 -> 3 -> 4 -> 5 — [Constraint 1]
-- >            | -> 6 -> 7 — [Constraint 2]
--
-- which you use to unfold everything in 1, 2, and 3 once (instead of twice)
-- and with the proper existing environment
--
mkCTrie :: [(SubcId, SimpC a)] -> CTrie
mkCTrie ics  = T.fromList [ (cBinds c, i) | (i, c) <- ics ]
  where
    cBinds   = L.sort . elemsIBindEnv . senv

----------------------------------------------------------------------------------------------
-- | Step 2: @pleTrie@ walks over the @CTrie@ to actually do the incremental-PLE
pleTrie :: Loc a => CTrie -> InstEnv a -> SmtM InstRes
pleTrie t env = loopT env ctx0 diff0 Nothing res0 t
  where
    diff0        = []
    res0         = M.empty
    ctx0         = ICtx
      { icAssms              = mempty
      , icCands              = mempty
      , icEquals             = mempty
      , icSimpl              = mempty
      , icSubcId             = Nothing
      , icANFs               = []
      , icLRWs               = mempty
      , icBindIds            = mempty
      , icEtaBetaFlag        = etabeta        $ ieCfg env
      , icExtensionalityFlag = extensionality $ ieCfg env
      , icLocalRewritesFlag  = localRewrites  $ ieCfg env
      , icFreshExistentialCounter = 0
      , icInitialLHSs  = mempty
      }

loopT
  :: Loc a
  => InstEnv a
  -> ICtx
  -> Diff         -- ^ The longest path suffix without forks in reverse order
  -> Maybe BindId -- ^ bind id of the branch ancestor of the trie if any.
                  --   'Nothing' when this is the top-level trie.
  -> InstRes
  -> CTrie
  -> SmtM InstRes
loopT env ictx delta i res t = case t of
  T.Node []  -> return res
  T.Node [b] -> loopB env ictx delta i res b
  T.Node bs  -> withAssms env ictx delta Nothing (Just t) $ \ictx' -> do
                  (ictx'', env'', res') <- ple1 env ictx' i res
                  foldM (loopB env'' ictx'' [] i) res' bs

loopB
  :: Loc a
  => InstEnv a
  -> ICtx
  -> Diff         -- ^ The longest path suffix without forks in reverse order
  -> Maybe BindId -- ^ bind id of the branch ancestor of the branch if any.
                  --   'Nothing' when this is a branch of the top-level trie.
  -> InstRes
  -> CBranch
  -> SmtM InstRes
loopB env ictx delta iMb res b = case b of
  T.Bind i t -> loopT env ictx (i:delta) (Just i) res t
  T.Val cid  -> withAssms env ictx delta (Just cid) Nothing $ \ictx' -> do
                  liftIO progressTick
                  (\(_, _, r) -> r) <$> ple1 env ictx' iMb res

collectConstraints :: CTrie -> [SubcId]
collectConstraints = go
  where
    go (T.Node bs) = concatMap goB bs
    goB (T.Bind _ t) = go t
    goB (T.Val cid)  = [cid]

-- | Adds to @ctx@ candidate expressions to unfold from the bindings in @delta@
-- and the rhs of @cidMb@.
--
-- Adds to @ctx@ assumptions from @env@ and @delta@.
--
-- Sets the current constraint id in @ctx@ to @cidMb@.
--
-- Pushes assumptions from the modified context to the SMT solver, runs @act@,
-- and then pops the assumptions.
--
withAssms
  :: Loc a
  => InstEnv a
  -> ICtx
  -> Diff
  -> Maybe SubcId
  -> Maybe CTrie
  -> (ICtx -> SmtM b)
  -> SmtM b
withAssms env ctx delta cidMb mCTrie act = do
  sctx <- get
  let cfg = SMT.config sctx
  let (ictx', bs) = updCtx cfg env sctx ctx delta cidMb mCTrie
  let assms = icAssms ictx'

  SMT.smtBracket "PLE.withAssms" $ do
    -- See Note [Existential quantification when unfolding]
    SMT.smtDecls $ elabBindings (ieEvEnv env) bs
    forM_ (S.toList assms) SMT.smtAssertDecl
    act $ ictx' { icAssms = mempty }

  where
    elabBindings eenv bs =
      elaborate (ElabParam (evElabF eenv) "withAssms: PExist Args" (evEnv eenv)) bs

-- | @ple1@ performs the PLE at a single "node" in the Trie
--
-- It will generate equalities for all function invocations in the candidates
-- in @ctx@ for which definitions are known. The function definitions are in
-- @ieKnowl@.
ple1 :: InstEnv a -> ICtx -> Maybe BindId -> InstRes -> SmtM (ICtx, InstEnv a, InstRes)
ple1 ie@InstEnv{..} ictx i res = do
  ctx <- get
  (ictx', env) <- liftIO $ runStateT (evalCandsLoop ieCfg ictx ieKnowl) (ieEvEnv { evKCtx = ctx })
  put $ evKCtx env
  let pendings = collectPendingUnfoldings env (icSubcId ictx)
      newEqs =
        reconstructExistentials
          (M.intersectionWith S.union (icInitialLHSs ictx) $         -- add original predicates
           M.map (S.map equalitiesPred) $                            -- construct equalities
           M.unionWith S.union pendings $                            -- pending unfoldings if any
           M.unionWith S.difference (icEquals ictx') (icEquals ictx) -- new equalities only
          )
  return (ictx', ie { ieEvEnv = env }, updCtxRes res i newEqs)
  where
    -- Pending unfoldings (i.e. with undecided guards) are collected only
    -- when we reach a leaf in the Trie, and only if the user asked for them.
    collectPendingUnfoldings env (Just _) | pleUndecGuards ieCfg =
      M.map (S.fromList . M.toList) (evPendingUnfoldings env)
    collectPendingUnfoldings _ _ = mempty

evalToSMT :: String -> Config -> SMT.Context -> [(Symbol, Sort)] -> (Expr, Expr) -> Pred
evalToSMT msg cfg ctx bs (e1,e2) = toSMT ("evalToSMT:" ++ msg) cfg ctx bs (EEq e1 e2)

-- | Generate equalities for all function invocations in the candidates
-- in @ctx@ for which definitions are known. The function definitions are in
-- @ieKnowl@.
--
-- In pseudocode:
--
-- > do
-- >     for every candidate
-- >         discover equalities,
-- >         unfold function invocations,
-- >         update candidates with the unfolded expressions
-- >     send newly discovered equalities to the SMT solver
-- > until no new equalities are discovered
-- >       or the environment becomes inconsistent
--
evalCandsLoop :: Config -> ICtx -> Knowledge -> EvalST ICtx
evalCandsLoop cfg ictx0 γ = go ictx0 0
  where
    go :: ICtx -> Int -> EvalST ICtx
    go ictx _ | all null (icCands ictx) = return ictx
    go ictx i = do
      inconsistentEnv <- testForInconsistentEnvironment
      if inconsistentEnv
        then return ictx
        else do liftSMT $ SMT.smtAssertDecl $ pAndNoDedup $ S.toList $ icAssms ictx
                let ictx' = ictx { icAssms = mempty }
                    (scopes, candSets) = unzip $ M.toList $ icCands ictx
                    cands = map S.toList candSets
                (candss, uss) <- unzip <$> zipWithM (evalCand ictx' i) scopes cands
                let noCandidateChanged = all and $ zipWith (zipWith eqCand) candss cands
                    unknownEqs = M.unionWith S.difference (M.fromList (zip scopes uss)) (icEquals ictx)
                if all null unknownEqs && noCandidateChanged then
                  return ictx
                else do
                  ctx' <- gets evKCtx
                  let eqsSMT =
                        S.unions $ M.elems $
                          M.mapWithKey
                            (\scope -> S.map $ evalToSMT "evalCandsLoop" cfg ctx' scope)
                            unknownEqs
                      ictx'' = ictx
                        { icEquals = M.unionWith S.union (icEquals ictx) unknownEqs
                        , icAssms  = S.filter (not . isTautoPred) eqsSMT
                        }
                  go (ictx'' { icCands = M.fromList $ zip scopes (map (S.fromList . concat) candss) }) (i + 1)

    testForInconsistentEnvironment :: EvalST Bool
    testForInconsistentEnvironment =
      knPredsEvalST γ PFalse

    eqCand [e0] e1 = e0 == e1
    eqCand _ _ = False

    evalCand :: ICtx -> Int -> ExScope -> [Expr] -> EvalST ([[Expr]], S.HashSet (Expr, Expr))
    evalCand ictx i scope es = withExScope scope $ mapM (evalOne γ ictx i) es >>= collectEqs

    collectEqs :: [[Expr]] -> EvalST ([[Expr]], S.HashSet (Expr, Expr))
    collectEqs es = do
      env <- get
      let newEqs = evNewEqualities env
      modify $ \st -> st { evNewEqualities = mempty }
      return (es, newEqs)

    withExScope :: ExScope -> EvalST a -> EvalST a
    withExScope s m = do
      env <- get
      put $ env { evExScope = s }
      r <- m
      modify $ \st -> st { evExScope = evExScope env }
      return r


----------------------------------------------------------------------------------------------
-- | Step 3: @resSInfo@ uses incremental PLE result @InstRes@ to produce the strengthened SInfo
----------------------------------------------------------------------------------------------

resSInfo :: Config -> SymEnv -> SInfo a -> InstRes -> BindEnv a
resSInfo cfg env info res = strengthenBinds info res'
  where
    res'     = M.fromList $ zip is ps''
    ps''     = zipWith (\i -> elaborate (ElabParam (solverFlags cfg) (atLoc dummySpan ("PLE1 " ++ show i)) env)) is ps'
    ps'      = defuncAny cfg env ps
    (is, ps) = unzip (M.toList res)

----------------------------------------------------------------------------------------------
-- | @InstEnv@ has the global information needed to do PLE
----------------------------------------------------------------------------------------------

data InstEnv a = InstEnv
  { ieCfg   :: !Config
  , ieBEnv  :: !(BindEnv a)
  , ieAenv  :: !AxiomEnv
  , ieCstrs :: !(CMap (SimpC a))
  , ieKnowl :: !Knowledge
  , ieEvEnv :: !EvalEnv
  , ieLRWs  :: LocalRewritesEnv
  , ieSol :: Maybe Solution
  }

----------------------------------------------------------------------------------------------
-- | @ICtx@ is the local information -- at each trie node -- obtained by incremental PLE
----------------------------------------------------------------------------------------------

data ICtx    = ICtx
  { icAssms              :: S.HashSet Pred           -- ^ Equalities converted to SMT format
  , icCands              :: M.HashMap ExScope (S.HashSet Expr)  -- ^ "Candidates" for unfolding
  , icEquals             :: M.HashMap ExScope EvEqualities      -- ^ Accumulated equalities
  , icSimpl              :: !ConstMap                -- ^ Map of expressions to constants
  , icSubcId             :: Maybe SubcId             -- ^ Current subconstraint ID
  , icANFs               :: [[(Symbol, SortedReft)]] -- Hopefully contain only ANF things
  , icLRWs               :: LocalRewrites            -- ^ Local rewrites
  , icBindIds            :: IBindEnv                 -- ^ Bind Ids in the current context
  , icEtaBetaFlag        :: Bool                     -- ^ True if the etabeta flag is turned on, needed
                                                     -- for the eta expansion reasoning as its going to
                                                     -- generate ho constraints
                                                     -- See Note [Eta expansion].
  , icExtensionalityFlag :: Bool                     -- ^ True if the extensionality flag is turned on
  , icLocalRewritesFlag  :: Bool                     -- ^ True if the local rewrites flag is turned on
  , icFreshExistentialCounter :: Int                 -- ^ Counter to generate fresh names for existentials
  , icInitialLHSs :: M.HashMap ExScope (S.HashSet Expr)
                                                     -- ^ LHS candidates before any unfoldings
  }

----------------------------------------------------------------------------------------------
-- | @InstRes@ is the final result of PLE; a map from @BindId@ to the equations "known" at that BindId
----------------------------------------------------------------------------------------------

type InstRes = M.HashMap BindId Expr

----------------------------------------------------------------------------------------------
-- | @Unfold is the result of running PLE at a single equality;
--     (e, [(e1, e1')...]) is the source @e@ and the (possible empty)
--   list of PLE-generated equalities (e1, e1') ...
----------------------------------------------------------------------------------------------

type CTrie   = T.Trie   SubcId
type CBranch = T.Branch SubcId
type Diff    = [BindId]    -- ^ in "reverse" order

equalitiesPred :: (Expr, Expr) -> Expr
equalitiesPred (e1, e2)
  | e1 /= e2 = EEq e1 e2
  | otherwise = PTrue

updCtxRes :: InstRes -> Maybe BindId -> [Expr] -> InstRes
updCtxRes res iMb = updRes res iMb . pAndNoDedup


updRes :: InstRes -> Maybe BindId -> Expr -> InstRes
updRes res (Just i) e = M.insertWith (error "tree-like invariant broken in ple. See https://github.com/ucsd-progsys/liquid-fixpoint/issues/496") i e res
updRes res  Nothing _ = res

----------------------------------------------------------------------------------------------
-- | @updCtx env ctx delta cidMb@ adds the assumptions and candidates from @delta@ and @cidMb@
--   to the context.
--
-- Yields the new context and a list of existential binders found in @delta@.
-- See Note [Existential quantification when unfolding].
----------------------------------------------------------------------------------------------

updCtx
  :: Loc a
  => Config
  ->InstEnv a
  -> SMT.Context
  -> ICtx
  -> Diff
  -> Maybe SubcId
  -> Maybe CTrie
  -> (ICtx, [(Symbol, Sort)])
updCtx cfg InstEnv{..} ieSMT ictx delta cidMb mCTrie =
    ( ictx { icAssms  = S.fromList ctxEqs
           , icCands  = M.unionWith S.union candsPerExScope (icCands ictx)
           , icSimpl  = icSimpl ictx <> econsts
           , icSubcId = cidMb
           , icANFs   = anfBinds
           , icLRWs   = mconcat $ icLRWs ictx : newLRWs
           , icBindIds = ibinds
           , icFreshExistentialCounter = existentialCounter
           , icInitialLHSs = M.unionWith S.union candsPerExScopeNoRHS (icInitialLHSs ictx)
           }
    , ebs
    )
  where
    ebs = concat (M.keys candsPerExScope)
    ibinds = insertsIBindEnv delta (icBindIds ictx)
    cands     = rhs:es
    anfBinds  = bs : icANFs ictx
    econsts   = M.fromList $ findConstants ieKnowl es
    ctxEqs    = toSMT "updCtx" ieCfg ieSMT ebs <$> L.nub
                  [ c
                  | (_, s) <- drop 1 deANFedCands
                  , e <- S.toList s
                  , c <- conjuncts e
                  , not (isTautoPred c)
                  ]
    bs        = second unApplySortedReft <$> binds
    rhs       = unApply eRhs
    es        = expr <$> bs
    eRhs      = maybe PTrue crhs subMb

    (binds, existentialCounter) = renameExistentialsInSortedRefts binds0 (icFreshExistentialCounter ictx)

    binds0    = [ maybeApplyKVarSolutions (x, y)
                | i <- delta
                , let (x, y, _) = lookupBindEnv i ieBEnv
                ]
    subMb     = getCstr ieCstrs <$> cidMb
    newLRWs   = Mb.mapMaybe (`lookupLocalRewrites` ieLRWs) delta

    candsPerExScopeNoRHS = M.fromListWith S.union $ ([], S.empty) : drop 1 deANFedCands
    -- ebs expects all keys to contain disjoint sets of bindings
    candsPerExScope = M.unionWith S.union candsPerExScopeNoRHS $ M.fromListWith S.union (take 1 deANFedCands)

    deANFedCands = map (second S.singleton . prenexExistentials) $
      -- We only call 'deANF' if necessary.
      if not (null (getAutoRws ieKnowl cidMb))
         || icExtensionalityFlag ictx
         || icEtaBetaFlag ictx then
        deANF anfBinds cands
      else
        cands

    maybeApplyKVarSolutions xsr =
      case ieSol of
        Just sol -> applyInSortedReft cfg g sol xsr
        Nothing  -> xsr
      where
        gCid = case collectConstraints <$> mCTrie of
          Just (c:_) -> Just c
          _ -> Nothing
        g = CEnv
          { ceCid = gCid
          , ceBEnv = ieBEnv
          , ceIEnv = ibinds
          , ceSpan = maybe dummySpan srcSpan $ gCid >>= (`M.lookup` ieCstrs)
          , ceBindingsInSmt = emptyIBindEnv
          }


findConstants :: Knowledge -> [Expr] -> [(Expr, Expr)]
findConstants γ es = [(EVar x, c) | (x,c) <- go [] (concatMap splitPAnd es)]
  where
    go su ess = if ess == ess'
                  then su
                  else go (su ++ su') ess'
       where ess' = subst (mkSubst su') <$> ess
             su'  = makeSu ess
    makeSu exprs  = [(x,c) | (EEq (EVar x) c) <- exprs
                           , isConstant (knDCs γ) c
                           , EVar x /= c ]

getCstr :: M.HashMap SubcId (SimpC a) -> SubcId -> SimpC a
getCstr env cid = Misc.safeLookup "Instantiate.getCstr" cid env

isPleCstr :: AxiomEnv -> SubcId -> SimpC a -> Bool
isPleCstr aenv subid c = isTarget c && M.lookupDefault False subid (aenvExpand aenv)

type EvEqualities = S.HashSet (Expr, Expr)

--------------------------------------------------------------------------------
data EvalEnv = EvalEnv
  { evEnv      :: !SymEnv
  , evElabF    :: ElabFlags
  , evKCtx     :: SMT.Context
    -- | The current scope of existential variables.
    -- See Note [Existential quantification when unfolding].
  , evExScope  :: ExScope
    -- | Equalities where we couldn't evaluate the guards, in a map which
    -- uses their existential scope as key.
    --
    -- See Note [Existential quantification when unfolding].
  , evPendingUnfoldings :: M.HashMap ExScope (M.HashMap Expr Expr)
  , evNewEqualities :: EvEqualities -- ^ Equalities discovered during a traversal of
                                    -- an expression
  , evSMTCache :: M.HashMap Expr Bool -- ^ Whether an expression is valid or its negation
  , evFuel     :: FuelCount

  -- Eta expansion feature
  , freshEtaNames :: Int -- ^ Keeps track of how many names we generated to perform eta
                         --   expansion, we use this to generate always fresh names
  -- REST parameters
  , explored   :: Maybe (ExploredTerms RuntimeTerm OCType IO)
  , restSolver :: Maybe SolverHandle
  , restOCA    :: RESTOrdering
  , evOCAlgebra :: OCAlgebra OCType RuntimeTerm IO
  }

data FuelCount = FC
  { fcMap :: M.HashMap Symbol Int
  , fcMax :: Maybe Int
  }
  deriving (Show)

defFuelCount :: Config -> FuelCount
defFuelCount cfg = FC mempty (fuel cfg)

type EvalST a = StateT EvalEnv IO a

liftSMT :: SmtM a -> EvalST a
liftSMT k =
  do es <- get
     let ctx = evKCtx es
     (a, ctx') <- lift $ runStateT k ctx
     put (es {evKCtx = ctx'})
     pure a

--------------------------------------------------------------------------------

getAutoRws :: Knowledge -> Maybe SubcId -> [AutoRewrite]
getAutoRws γ mSubcId =
  Mb.fromMaybe [] $ do
    cid <- mSubcId
    M.lookup cid $ knAutoRWs γ

-- | Discover the equalities in an expression.
--
-- The discovered equalities are in the environment of the monad,
-- and the list of produced expressions contains the result of unfolding
-- definitions. When REST is in effect, more than one expression might
-- be returned because expressions can then be rewritten in more than one
-- way.
evalOne :: Knowledge -> ICtx -> Int -> Expr -> EvalST [Expr]
evalOne γ ctx i e
  | i > 0 || null (getAutoRws γ (icSubcId ctx)) = (:[]) <$> eval γ ctx NoRW e
evalOne γ ctx _ e | isExprRewritable e = do
    env <- get
    let oc :: OCAlgebra OCType RuntimeTerm IO
        oc = evOCAlgebra env
        rp = RP (contramap Rewrite.convert oc) [(e, PLE)] constraints
        constraints = OC.top oc
        emptyET = ExploredTerms.empty (EF (OC.union oc) (OC.notStrongerThan oc) (OC.refine oc)) ExploreWhenNeeded
    es <- evalREST γ ctx rp
    modify $ \st -> st { explored = Just emptyET }
    return es
evalOne _ _ _ _ = return []

-- The FuncNormal and RWNormal evaluation strategies are used for REST
-- For example, consider the following function:
--   add(x, y) = if x == 0 then y else add(x - 1, y + 1)
-- And a rewrite rule:
--   forall a, b . add(a,b) -> add(b, a)
-- Then the expression add(t, add(2, 1)) would evaluate under NoRW to:
--   if t == 0 then 3 else add(t - 1, 4)
-- However, under FuncNormal, it would evaluate to: add(t, 3)
-- Thus, FuncNormal could engage the rewrite rule add(t, 3) = add(3, t)


data EvalType =
    NoRW       -- Normal PLE
  | FuncNormal -- REST: Expand function definitions only when the branch can be decided
  | RWNormal   -- REST: Fully Expand Defs in the context of rewriting (similar to NoRW)
  deriving (Eq)


-- | Unfolds function invocations in expressions.
--
-- Also reduces if-then-else when the boolean condition or the negation can be
-- proved valid. This is the actual implementation of guard-validation-before-unfolding
-- that is described in publications.
--
-- Also adds to the monad state all the unfolding equalities that have been
-- discovered as necessary.
eval :: Knowledge -> ICtx -> EvalType -> Expr -> EvalST Expr
eval γ ctx et = go
  where
    go (ELam (x,s) e)   = evalELam γ ctx et (x, s) e
    go e@EIte{}         = evalIte γ ctx et e
    go (ECoerc s t e)   = ECoerc s t <$> go e
    go e@(EApp _ _)     =
      case splitEAppThroughECst e of
       (f, es) | et == RWNormal ->
          -- Just evaluate the arguments first, to give rewriting a chance to step in
          -- if necessary
          do
            es' <- mapM (eval γ ctx et) es
            if es /= es'
              then return (eApps f es')
              else do
                f' <- case dropECst f of
                  EVar _ -> pure f
                  _      -> go f
                Mb.fromMaybe (eApps f' es') <$> evalApp γ ctx f' es et
       (f, es) ->
          do
            f' <- case dropECst f of
              EVar _ -> pure f
              _      -> go f
            es' <- mapM (eval γ ctx et) es
            Mb.fromMaybe (eApps f' es') <$> evalApp γ ctx f' es' et

    go (PAtom r e1 e2) = PAtom r <$> go e1 <*> go e2
    go (ENeg e)         = ENeg <$> go e
    go (EBin o e1 e2)   = EBin o <$> go e1 <*> go e2
    go (ETApp e t)      = (`ETApp` t) <$> go e
    go (ETAbs e s)      = (`ETAbs` s) <$> go e
    go (PNot e')        = PNot <$> go e'
    go (PImp e1 e2)     = PImp <$> go e1 <*> go e2
    go (PIff e1 e2)     = PIff <$> go e1 <*> go e2
    go (PAnd es)        = PAnd <$> traverse go es
    go (POr es)         = POr <$> traverse go es
    go e | EVar _ <- dropECst e = do
      Mb.fromMaybe e <$> evalApp γ ctx e [] et
    go (ECst e t)       = (`ECst` t) <$> go e
    go (ELet x e1 e2)   = ELet x <$> go e1 <*> go e2

    go e                = return e


-- | 'evalELam' produces equations that preserve the context of a rewrite
-- so equations include any necessary lambda bindings.
evalELam :: Knowledge -> ICtx -> EvalType -> (Symbol, Sort) -> Expr -> EvalST Expr
evalELam γ ctx et (x, s) e
  | not $ isEtaSymbol x = do
    -- We need to refresh it as for some reason names bound by lambdas
    -- present in the source code are getting declared twice.
    -- Maybe we should define a new type of identifier for this kind of fresh
    -- variables and not reuse the etabeta ones.
    [ xFresh ] <- makeFreshEtaNames 1
    let newBody = subst (mkSubst [(x, EVar xFresh)]) e

    modify $ \st -> st
      { evNewEqualities
        = S.insert (ELam (x, s) e, ELam (xFresh, s) newBody)
                   (evNewEqualities st)
      }

    evalELam γ ctx et (xFresh, s) newBody
  where
    isEtaSymbol :: Symbol -> Bool
    isEtaSymbol = isPrefixOfSym "eta"

evalELam γ ctx et (x, s) e = do
    oldPendingUnfoldings <- gets evPendingUnfoldings
    oldEqs <- gets evNewEqualities

    -- We need to declare the variable in the environment
    modify $ \st -> st
      { evEnv = insertSymEnv x s $ evEnv st }

    e' <- eval (γ { knLams = (x, s) : knLams γ }) ctx et e
    let e2' = simplify γ ctx e'
        elam = ELam (x, s) e
    -- Discard the old equalities which miss the lambda binding
    modify $ \st -> st
      { evPendingUnfoldings = oldPendingUnfoldings
      , evNewEqualities = S.insert (elam, ELam (x, s) e2') oldEqs
      -- Leaving the scope thus we need to get rid of it
      , evEnv = deleteSymEnv x $ evEnv st
      }
    return (ELam (x, s) e')

data RESTParams oc = RP
  { oc   :: OCAlgebra oc Expr IO
  , path :: [(Expr, TermOrigin)]
  , c    :: oc
  }

-- An expression is rewritable if it is in the domain of
-- Language.Fixpoint.Solver.Rewrite.convert
isExprRewritable :: Expr -> Bool
isExprRewritable (EIte i t e ) = isExprRewritable i && isExprRewritable t && isExprRewritable e
isExprRewritable (EApp f e) = isExprRewritable f && isExprRewritable e
isExprRewritable (EVar _) = True
isExprRewritable (PNot e) = isExprRewritable e
isExprRewritable (PAnd es) = all isExprRewritable es
isExprRewritable (POr es) = all isExprRewritable es
isExprRewritable (PAtom _ l r) = isExprRewritable l && isExprRewritable r
isExprRewritable (EBin _ l r) = isExprRewritable l && isExprRewritable r
isExprRewritable (ECon _) = True
isExprRewritable (ESym _) = True
isExprRewritable (ECst _ _) = True
isExprRewritable (PIff e0 e1) = isExprRewritable (PAtom Eq e0 e1)
isExprRewritable (PImp e0 e1) = isExprRewritable (POr [PNot e0, e1])
isExprRewritable _ = False

-- | Reverse the ANF transformation
--
-- This is necessary for REST rewrites, beta reduction, and PLE to discover
-- redexes.
--
-- In the case of REST, ANF bindings could hide compositions that are
-- rewriteable. For instance,
--
-- > let anf1 = map g x
-- >  in map f anf1
--
-- could miss a rewrite like @map f (map g x) ~> map (f . g) x@.
--
-- Similarly, ANF bindings could miss beta reductions. For instance,
--
-- > let anf1 = \a b -> b
-- >  in anf1 x y
--
-- could only be reduced by PLE if @anf1@ is inlined.
--
-- Lastly, in the following example PLE cannot unfold @reflectedFun@ unless the
-- ANF binding is inlined.
--
-- > f g = g 0
-- > reflectedFun x y = if y == 0 then x else y
-- >
-- > let anf2 = (\eta1 -> reflectedFun x eta1)
-- >  in f anf2
--
-- unfolding @f@
--
-- > let anf2 = (\eta1 -> reflectedFun x eta1)
-- >  in anf2 0
--
deANF :: [[(Symbol, SortedReft)]] -> [Expr] -> [Expr]
deANF binds = map $ inlineInExpr (`HashMap.Lazy.lookup` bindEnv)
  where
    bindEnv = undoANF id
        $ HashMap.Lazy.filterWithKey (\sym _ -> anfPrefix `isPrefixOfSym` sym)
        $ HashMap.Lazy.unions $ map HashMap.Lazy.fromList binds

-- |
-- Adds to the monad state all the subexpressions that have been rewritten
-- as pairs @(original_subexpression, rewritten_subexpression)@.
--
-- Also folds constants.
--
-- The main difference with 'eval' is that 'evalREST' takes into account
-- autorewrites.
--
evalREST :: Knowledge -> ICtx -> RESTParams OCType -> EvalST [Expr]
evalREST γ ctx rp = do
  env <- get
  cacheRef <- liftIO $ newIORef $ evSMTCache env
  evalRESTWithCache cacheRef γ ctx [] rp

evalRESTWithCache
  :: IORef (M.HashMap Expr Bool) -> Knowledge -> ICtx -> [Expr] -> RESTParams OCType -> EvalST [Expr]
evalRESTWithCache cacheRef _ ctx acc rp
  | pathExprs <- map fst (mytracepp "EVAL1: path" $ path rp)
  , e         <- last pathExprs
  , Just v    <- M.lookup e (icSimpl ctx)
  = do
    smtCache <- liftIO $ readIORef cacheRef
    when (v /= e) $ modify (\st -> st
      { evNewEqualities = S.insert (e, v) (evNewEqualities st)
      , evSMTCache = smtCache
      })
    return (v : acc)

evalRESTWithCache cacheRef γ ctx acc rp =
  do
    mexploredTerms <- gets explored
    ebs <- gets evExScope
    case mexploredTerms of
      Nothing -> return acc
      Just exploredTerms -> do
        se <- liftIO (shouldExploreTerm ebs exploredTerms exprs)
        if se then do
          possibleRWs <- liftSMT (getRWs ebs)
          rws <- notVisitedFirst exploredTerms <$> filterM (liftIO . allowed ebs) possibleRWs
          oldEqualities <- gets evNewEqualities
          modify $ \st -> st { evNewEqualities = mempty }

          -- liftIO $ putStrLn $ (show $ length possibleRWs) ++ " rewrites allowed at path length " ++ (show $ (map snd $ path rp))
          e' <- do
            ec <- eval γ ctx FuncNormal exprs
            if ec /= exprs
              then return ec
              else eval γ ctx RWNormal exprs

          let evalIsNewExpr = e' `L.notElem` pathExprs
          let exprsToAdd    = [e' | evalIsNewExpr]  ++ map (\(_, e, _) -> e) rws
              acc' = exprsToAdd ++ acc
              eqnToAdd = [ (e1, simplify γ ctx e2) | ((e1, e2), _, _) <- rws ]

          let explored' st =
                if isExprRewritable e' && isExprRewritable exprs
                  then Just $ ExploredTerms.insert (Rewrite.convert exprs) (c rp)
                                                  (S.insert (Rewrite.convert e')
                            $ S.fromList (map (Rewrite.convert . (\(_, e, _) -> e)) possibleRWs))
                                        (Mb.fromJust $ explored st)
                  else Nothing

          newEqualities <- gets evNewEqualities
          smtCache <- liftIO $ readIORef cacheRef
          modify $ \st -> st
            { evNewEqualities  = foldr S.insert (S.union newEqualities oldEqualities) eqnToAdd
            , evSMTCache = smtCache
            , explored = explored' st
            }

          acc'' <- if evalIsNewExpr
            then if e' /= exprs && any isRW (path rp)
              then (:[]) <$> eval γ (addConst (exprs, e')) NoRW e'
              else evalRESTWithCache cacheRef γ (addConst (exprs, e')) acc' (rpEval newEqualities e')
            else return acc'

          foldM (\r rw -> evalRESTWithCache cacheRef γ ctx r (rpRW rw)) acc'' rws
        else
          return acc
  where
    shouldExploreTerm ebs exploredTerms e | Vis.isConc e =
      case rwTerminationOpts (rwArgs ebs) of
        RWTerminationCheckDisabled ->
          return $ not $ ExploredTerms.visited (Rewrite.convert e) exploredTerms
        RWTerminationCheckEnabled  ->
          ExploredTerms.shouldExplore (Rewrite.convert e) (c rp) exploredTerms
    shouldExploreTerm _ _ _ = return False

    allowed _ebs (_, rwE, _) | rwE `elem` pathExprs = return False
    allowed ebs (_, _, c)   = termCheck ebs c
    termCheck ebs c = Rewrite.passesTerminationCheck (oc rp) (rwArgs ebs) c

    notVisitedFirst exploredTerms rws =
      let
        (v, nv) = L.partition (\(_, e, _) -> ExploredTerms.visited (Rewrite.convert e) exploredTerms) rws
      in
        nv ++ v

    rpEval newEqualities e' =
      let
        c' =
          if any isRW (path rp)
            then foldr (\(e1, e2) ctrs -> refine (oc rp) ctrs e1 e2) (c rp) (S.toList newEqualities)
            else c rp

      in
        rp{path = path rp ++ [(e', PLE)], c = c'}

    isRW (_, r) = r == RW

    rpRW (_, e', c') = rp{path = path rp ++ [(e', RW)], c = c' }

    pathExprs       = map fst (mytracepp "EVAL2: path" $ path rp)
    exprs           = last pathExprs
    autorws         = getAutoRws γ (icSubcId ctx)

    rwArgs ebs = RWArgs (isValid cacheRef ebs γ) $ knRWTerminationOpts γ

    getRWs ebs =
      do
        -- Optimization: If we got here via rewriting, then the current constraints
        -- are satisfiable; otherwise double-check that rewriting is still allowed
        ok <-
          if isRW $ last (path rp)
            then return True
            else liftIO $ termCheck ebs (c rp)
        if ok
          then
            do
              let getRW e ar = Rewrite.getRewrite (oc rp) (rwArgs ebs) (c rp) e ar
              let getRWs' s  = Mb.catMaybes <$> mapM (runMaybeT . getRW s) autorws
              concat <$> mapM getRWs' (subExprs exprs)
          else return []

    addConst (e,e') = if isConstant (knDCs γ) e'
                      then ctx { icSimpl = M.insert e e' $ icSimpl ctx} else ctx

-- Note [Eta expansion]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Without eta expansion PLE could not prove that terms @f@ and @(\x -> f x)@
-- have the same meaning. But sometimes we want to rewrite @f@ into the
-- expanded form, in order to unfold @f@.
--
-- For instance, suppose we have a function @const@ defined as:
--
-- > define f (x : int, y : int) : int = {(x)}
--
-- And we need to prove some constraint of this shape
--
-- > { const a = \x:Int -> a }
--
-- At first, PLE cannot unfold @const@ since it is not fully applied.
-- But if instead perform eta expansion on the left hand side we obtain the
-- following equality
--
-- > { \y:Int -> const a y = \x:Int -> a}
--
-- And now PLE can unfold @const@ as the application is saturated
--
-- > { \y:Int -> a = \x:Int -> a}
--
-- We need the higerorder flag active as we are generating lambdas in
-- the equalities.


-- Note [Elaboration for eta expansion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Eta expansion needs to determine the arity and the type of arguments of a
-- function. For this sake, we make sure that when unfolding introduces new
-- expressions, these expressions get annotated with their types by calling
-- @elaborateExpr@.
--
-- This elaboration cannot be done ahead of time on equations, because then
-- type variables are instantiated to rigid constants that cannot be unified.
-- For instance, @id :: forall a. a -> a@ would be elaborated to
-- @id :: a#1 -> a#1@, and when used in an expression like @id True@, @a#1@
-- would not unify with @Bool@.


-- | @evalApp kn ctx e es@ unfolds expressions in @eApps e es@ using rewrites
-- and equations
evalApp :: Knowledge -> ICtx -> Expr -> [Expr] -> EvalType -> EvalST (Maybe Expr)
evalApp γ ctx e0 es et
  | EVar f <- dropECst e0
  , Just eq <- Map.lookup f (knAms γ)
  , length (eqArgs eq) <= length es
  = do
       env <- gets (seSort . evEnv)
       okFuel <- checkFuel f
       if okFuel && et /= FuncNormal then do
         let (es1, es2) = splitAt (length (eqArgs eq)) es
         -- See Note [Elaboration for eta expansion].
         let newE = substEq env eq es1
         newE' <- if icEtaBetaFlag ctx
                    then elaborateExpr "EvalApp unfold full: " newE
                    else pure newE

         e' <- evalIte γ ctx et newE'        -- TODO:FUEL this is where an "unfolding" happens, CHECK/BUMP counter
         let e2' = stripPLEUnfold e'
         let e3' = simplify γ ctx (eApps e2' es2)  -- reduces a bit the equations

         if hasUndecidedGuard e' && guardOf e' == guardOf newE' then do
           -- Don't unfold the expression if there is an if-then-else guarding
           -- it, just to preserve the size of further rewrites.
           -- If evalIte does any modifications, though, we do unfold in order
           -- to allow analysis of the resulting expression
           modify $ \st -> st
             { evPendingUnfoldings =
                 M.insertWith M.union (evExScope st) (M.singleton (eApps e0 es) e3') (evPendingUnfoldings st)
             }
           return Nothing
         else do
           useFuel f
           modify $ \st -> st
             { evNewEqualities = S.insert (eApps e0 es, e3') (evNewEqualities st)
             , evPendingUnfoldings = M.adjust (M.delete (eApps e0 es)) (evExScope st) (evPendingUnfoldings st)
             }
           return (Just $ eApps e2' es2)
       else return Nothing
  where
    -- At the time of writing, any function application wrapping an
    -- if-statement would have the effect of unfolding the invocation.
    -- However, using pleUnfold still has the advantage of not generating
    -- extra equations to unfold pleUnfold itself. Using pleUnfold also
    -- makes the intention of the user rather explicit.
    stripPLEUnfold e
      | (ef, [arg]) <- splitEAppThroughECst e
      , EVar f <- dropECst ef
      , f == "Language.Haskell.Liquid.ProofCombinators.pleUnfold"
      = arg
      | otherwise = e

    hasUndecidedGuard EIte{} = True
    hasUndecidedGuard _ = False

    guardOf (EIte g _ _) = Just g
    guardOf _ = Nothing

evalApp γ ctx e0 args@(e:es) _
  | EVar f <- dropECst e0
  , (d, as) <- splitEAppThroughECst e
  , EVar dc <- dropECst d
  , Just rws <- Map.lookup dc (knSims γ)
    -- User data measures aren't sent to the SMT solver because
    -- it knows already about selectors and constructor tests.
  , Just (rw, isUserDataSMeasure) <- L.find (\(rw, _) -> smName rw == f) rws
  , length as == length (smArgs rw)
  = do
    let newE = eApps (subst (mkSubst $ zip (smArgs rw) as) (smBody rw)) es
    when (isUserDataSMeasure == NoUserDataSMeasure) $
      modify $ \st -> st
        { evNewEqualities = S.insert (eApps e0 args, simplify γ ctx newE) (evNewEqualities st) }
    return (Just newE)

evalApp γ ctx e0 es _et
  | eqs@(_:_) <- noUserDataMeasureEqs γ (eApps e0 es)
  = do
       let eqs' = map (second $ simplify γ ctx) eqs
       modify $ \st ->
         st { evNewEqualities = foldr S.insert (evNewEqualities st) eqs' }
       return Nothing

evalApp γ ctx e0 es et
  | ELam (argName, _) body <- dropECst e0
  , lambdaArg:remArgs <- es
  , icEtaBetaFlag ctx || icExtensionalityFlag ctx
  = do
      isFuelOk <- checkFuel argName
      if isFuelOk
        then do
          useFuel argName
          let argSubst = mkSubst [(argName, lambdaArg)]
          let body' = subst argSubst body
          body'' <- evalIte γ ctx et body'
          let simpBody = simplify γ ctx (eApps body'' remArgs)
          modify $ \st ->
            st { evNewEqualities = S.insert (eApps e0 es, simpBody) (evNewEqualities st) }
          return (Just $ eApps body'' remArgs)
        else do
          return Nothing

evalApp _ ctx e0 es _
  | icLocalRewritesFlag ctx
  , EVar f <- dropECst e0
  , Just rw <- lookupRewrite f $ icLRWs ctx
  = do
      -- expandedTerm <- elaborateExpr "EvalApp rewrite local:" $ eApps rw es
      let expandedTerm = eApps rw es
      modify $ \st -> st
        { evNewEqualities = S.insert (eApps e0 es, expandedTerm) (evNewEqualities st) }
      return (Just expandedTerm)

evalApp _γ ctx e0 es _et
  -- We check the annotation instead of the equations in γ for two reasons.
  --
  -- First, we want to eta expand functions that might not be reflected. Suppose
  -- we have an uninterpreted function @f@, and we want to prove that
  -- @f == \a -> f a@. We can use eta expansion on the left-hand side to prove
  -- this.
  --
  -- Second, we need the type of the new arguments, which for some reason are
  -- sometimes instantiated in the equations to rigid types that we cannot
  -- instantiate to the types needed at the call site.
  -- See Note [Elaboration for eta expansion].
  --
  -- See Note [Eta expansion].
  --
  | ECst (EVar _f) sortAnnotation@FFunc{} <- e0
  , icEtaBetaFlag ctx
  , let expectedArgs = unpackFFuncs sortAnnotation
  , let nProvidedArgs = length es
  , let nArgsMissing = length expectedArgs - nProvidedArgs
  , nArgsMissing > 0
  = do
    let etaArgsType = drop nProvidedArgs expectedArgs
    -- Fresh names for the eta expansion
    etaNames <- makeFreshEtaNames nArgsMissing

    let etaVars = zipWith (\name ty -> ECst (EVar name) ty) etaNames etaArgsType
    let fullBody = eApps e0 (es ++ etaVars)
    let etaExpandedTerm = mkLams fullBody (zip etaNames etaArgsType)

    -- Note: we should always add the equality as etaNames is always non empty because the
    -- only way for etaNames to be empty is if the function is fully applied, but that case
    -- is already handled by the previous case of evalApp
    modify $ \st -> st
      { evNewEqualities = S.insert (eApps e0 es, etaExpandedTerm) (evNewEqualities st) }
    return (Just etaExpandedTerm)
  where
    unpackFFuncs (FFunc t ts) = t : unpackFFuncs ts
    unpackFFuncs _ = []

    mkLams subject binds = foldr ELam subject binds

evalApp _ _ctx _e0 _es _ = do
  return Nothing

-- | Evaluates if-then-else statements until they can't be evaluated anymore
-- or some other expression is found.
evalIte :: Knowledge -> ICtx -> EvalType -> Expr -> EvalST Expr
evalIte γ ctx et (ECst e t) = do
  (`ECst` t) <$> evalIte γ ctx et e
evalIte γ ctx et (EIte i e1 e2) = do
      b <- eval γ ctx et i
      b'  <- mytracepp ("evalEIt POS " ++ showpp (i, b)) <$> isValidCached γ b
      case b' of
        Just True -> evalIte γ ctx et e1
        Just False -> evalIte γ ctx et e2
        _ -> return (EIte b e1 e2)
evalIte _ _ _ e' = return e'

-- | Creates equations that explain how to rewrite a given constructor
-- application with all measures that aren't user data measures
noUserDataMeasureEqs :: Knowledge -> Expr -> [(Expr,Expr)]
noUserDataMeasureEqs γ e =
  [ (EApp (EVar $ smName rw) e, subst (mkSubst $ zip (smArgs rw) es) (smBody rw))
  | (ef, es) <- [splitEAppThroughECst e]
  , EVar f <- [dropECst ef]
  , Just rws <- [Map.lookup f (knSims γ)]
  , (rw, NoUserDataSMeasure) <- rws
  , length es == length (smArgs rw)
  ]

--------------------------------------------------------------------------------
-- | 'substEq' unfolds or instantiates an equation at a particular list of
--   argument values. We must also substitute the sort-variables that appear
--   as coercions. See tests/proof/ple1.fq
--------------------------------------------------------------------------------
substEq :: SEnv Sort -> Equation -> [Expr] -> Expr
substEq env eq es = subst su (substEqCoerce env eq es)
  where su = mkSubst $ zip (eqArgNames eq) es

substEqCoerce :: SEnv Sort -> Equation -> [Expr] -> Expr
substEqCoerce env eq es = Vis.applyCoSubV coSub $ eqBody eq
  where
    ts    = snd    <$> eqArgs eq
    sp    = panicSpan "mkCoSub"
    eTs   = sortExpr sp env <$> es
    coSub = mkCoSub env eTs ts

-- | @mkCoSub senv eTs xTs = su@ creates a substitution @su@ such that
-- @subst su xTs == eTs@.
--
-- The variables in the domain of the substitution are those that appear
-- as @FObj symbol@ in @xTs@.
mkCoSub :: SEnv Sort -> [Sort] -> [Sort] -> Vis.CoSubV
mkCoSub env eTs xTs = M.fromList [ (x, unite ys) | (x, ys) <- Misc.groupList xys ]
  where
    unite ts    = Mb.fromMaybe (uError ts) (unifyTo1 symToSearch ts)
    symToSearch = mkSearchEnv env
    uError ts   = panic ("mkCoSub: cannot build CoSub for " ++ showpp xys ++ " cannot unify " ++ showpp ts)
    xys :: [(Sort, Sort)]
    xys         = Misc.sortNub $ concat $ zipWith matchSorts xTs eTs

matchSorts :: Sort -> Sort -> [(Sort, Sort)]
matchSorts = go
  where
    go x@(FObj _)    {-FObj-} y    = [(x, y)]
    go x@(FVar _)    {-FObj-} y    = [(x, y)]
    go (FAbs _ t1)   (FAbs _ t2)   = go t1 t2
    go (FFunc s1 t1) (FFunc s2 t2) = go s1 s2 ++ go t1 t2
    go (FApp s1 t1)  (FApp s2 t2)  = go s1 s2 ++ go t1 t2
    go _             _             = []

--------------------------------------------------------------------------------

eqArgNames :: Equation -> [Symbol]
eqArgNames = map fst . eqArgs

isValidCached :: Knowledge -> Expr -> EvalST (Maybe Bool)
isValidCached γ e = do
  env <- get
  case M.lookup e (evSMTCache env) of
    Nothing -> do
      let isFreeInE (s, _) = not (S.member s (exprSymbolsSet e))
      b <- knPredsEvalST γ e
      if b
        then do
          when (all isFreeInE (knLams γ)) $
            put (env { evSMTCache = M.insert e True (evSMTCache env) })
          return (Just True)
        else do
          b2 <- knPredsEvalST γ (PNot e)
          if b2
            then do
              when (all isFreeInE (knLams γ)) $
                put (env { evSMTCache = M.insert e False (evSMTCache env) })
              return (Just False)
            else
              return Nothing

    mb -> return mb

--------------------------------------------------------------------------------
-- | Knowledge (SMT Interaction)
--------------------------------------------------------------------------------
data Knowledge = KN
  { -- | Rewrites rules came from match definitions
    --
    -- They are grouped by the data constructor that they unfold, and are
    -- augmented with an attribute that say whether they originate from a
    -- user data declaration.
    knSims              :: Map Symbol [(Rewrite, IsUserDataSMeasure)]
  , knAms               :: Map Symbol Equation -- ^ All function definitions
    -- | @knPreds γ bsInSMT xs e@ checks whether @e@ is valid under the
    -- assumptions that all variables in @bsInSMT@ are in the SMT solver,
    -- and that all variables in @xs@ need tp be declared in the SMT solver.
  , knPreds             :: [(Symbol, Sort)] -> [(Symbol, Sort)] -> Expr -> SmtM Bool
  , knLams              :: ![(Symbol, Sort)]
  , knSummary           :: ![(Symbol, Int)]     -- ^ summary of functions to be evaluates (knSims and knAsms) with their arity
  , knDCs               :: !(S.HashSet Symbol)  -- ^ data constructors drawn from Rewrite
  , knDataCtors         :: !(M.HashMap Symbol DataCtor) -- ^ data constructors by name
  , knSels              :: !SelectorMap
  , knConsts            :: !ConstDCMap
  , knAutoRWs           :: M.HashMap SubcId [AutoRewrite]
  , knRWTerminationOpts :: RWTerminationOpts
  }

-- | A type to express whether SMeasures originate from data definitions.
-- That is whether they are constructor tests, selectors, or something else.
data IsUserDataSMeasure = NoUserDataSMeasure | UserDataSMeasure
  deriving (Eq, Show)

knPredsEvalST :: Knowledge -> Expr -> EvalST Bool
knPredsEvalST γ e = do
    env <- get
    liftSMT $ knPreds γ (evExScope env) (knLams γ) e

isValid :: IORef (M.HashMap Expr Bool) -> [(Symbol, Sort)] -> Knowledge -> Expr -> SmtM Bool
isValid cacheRef bs γ e = do
    smtCache <- liftIO $ readIORef cacheRef
    case M.lookup e smtCache of
      Nothing -> do
        b <- knPreds γ bs (knLams γ) e
        when b $
          liftIO $ writeIORef cacheRef (M.insert e True smtCache)
        return b
      Just b -> return b

knowledge :: Config -> SInfo a -> Knowledge
knowledge cfg si = KN
  { knSims                     = Map.fromListWith (++) $
                                   [ (smDC rw, [(rw, NoUserDataSMeasure)]) | rw <- sims ] ++
                                   [ (smDC rw, [(rw, UserDataSMeasure)]) | rw <- dataSims ]
  , knAms                      = Map.fromList [(eqName eq, eq) | eq <- aenvEqs aenv]
  , knPreds                    = askSMT cfg
  , knLams                     = []
  , knSummary                  =    ((\s -> (smName s, 1)) <$> sims)
                                 ++ ((\s -> (eqName s, length (eqArgs s))) <$> aenvEqs aenv)
                                 ++ rwSyms
  , knDCs                      = S.fromList (smDC <$> sims)
  , knDataCtors                = M.fromList [ (val (dcName dc), dc) | dd <- ddecls si, dc <- ddCtors dd ]
  , knSels                     = Mb.mapMaybe makeSel  sims
  , knConsts                   = Mb.mapMaybe makeCons sims
  , knAutoRWs                  = aenvAutoRW aenv
  , knRWTerminationOpts        =
      if rwTermination cfg
      then RWTerminationCheckEnabled
      else RWTerminationCheckDisabled
  }
  where
    (simDCTests, sims0) =
      partitionUserDataConstructorTests (ddecls si) $ aenvSimpl aenv
    (simDCSelectors, sims) =
      partitionUserDataConstructorSelectors (ddecls si) sims0
    dataSims = simDCTests ++ simDCSelectors
    aenv = ae si

    inRewrites :: Symbol -> Bool
    inRewrites e =
      let
        symbs = Mb.mapMaybe (lhsHead . arLHS) (concat $ M.elems $ aenvAutoRW aenv)
      in
        e `L.elem` symbs

    lhsHead :: Expr -> Maybe Symbol
    lhsHead e | (ef, _) <- splitEAppThroughECst e, EVar f <- dropECst ef = Just f
    lhsHead _ = Nothing


    rwSyms = filter (inRewrites . fst) $ map toSum (toListSEnv (gLits si))
      where
        toSum (sym, sort)      = (sym, getArity sort)

        getArity (FFunc _ rhs) = 1 + getArity rhs
        getArity _             = 0



    makeCons rw
      | null (syms $ smBody rw)
      = Just (smName rw, (smDC rw, smBody rw))
      | otherwise
      = Nothing

    makeSel rw
      | EVar x <- smBody rw
      = (smName rw,) . (smDC rw,) <$> L.elemIndex x (smArgs rw)
      | otherwise
      = Nothing

-- | Partitions the input rewrites into constructor tests and others.
--
-- We don't need to deal in PLE with data constructor tests. That is,
-- functions of the form @isCons :: List a -> Bool@ or @isNil :: List a -> Bool@
-- when @List a@ is defined by the user.
--
-- The SMT solver knows about these functions when datatypes are declared to it,
-- so PLE doesn't need to unfold them.
--
-- Non-user defined datatypes like @[a]@ still need to have tests unfolded
-- because they are not declared as datatypes to the SMT solver.
--
-- Also, REST could need this functions unfolded since otherwise it may not
-- discover possible rewrites.
--
partitionUserDataConstructorTests :: [DataDecl] -> [Rewrite] -> ([Rewrite], [Rewrite])
partitionUserDataConstructorTests dds rws = L.partition isDataConstructorTest rws
  where
    isDataConstructorTest sm = isTestSymbol (smName sm) && S.member (smDC sm) userDefinedDcs
    userDefinedDcs =
      S.fromList [ symbol (dcName dc) | dd <- dds, dc <- ddCtors dd ]

-- | Like 'partitionUserDataConstructorTests' but for selectors.
partitionUserDataConstructorSelectors :: [DataDecl] -> [Rewrite] -> ([Rewrite], [Rewrite])
partitionUserDataConstructorSelectors dds rws = L.partition isSelector rws
  where
    isSelector sm = S.member (smName sm) userDefinedDcFieldsSelectors
    userDefinedDcFieldsSelectors =
      S.fromList [ symbol dcf | dd <- dds, dc <- ddCtors dd, dcf <- dcFields dc ]


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- (sel_i, D, i), meaning sel_i (D x1 .. xn) = xi,
-- i.e., sel_i selects the ith value for the data constructor D
type SelectorMap = [(Symbol, (Symbol, Int))]
type ConstDCMap = [(Symbol, (Symbol, Expr))]

-- ValueMap maps expressions to constants (including data constructors)
type ConstMap = M.HashMap Expr Expr
type LDataCon = Symbol              -- Data Constructors

isConstant :: S.HashSet LDataCon -> Expr -> Bool
isConstant dcs e = S.null (S.difference (exprSymbolsSet e) dcs)

simplify :: Knowledge -> ICtx -> Expr -> Expr
simplify γ ictx exprs = mytracepp ("simplification of " ++ showpp exprs) $ fix' (Vis.mapExprOnExpr tx) exprs
    where
      fix' f e = if e == e' then e else fix' f e' where e' = f e
      tx e
        | Just e' <- M.lookup e (icSimpl ictx)
        = e'

      tx (PAtom rel e1 e2) = applyBooleanFolding rel e1 e2
      tx (EBin bop e1 e2) = applyConstantFolding bop e1 e2
      tx (ENeg e)         = applyConstantFolding Minus (ECon (I 0)) e
      tx (EApp e1 e2)
        | isSetPred e1    = applySetFolding e1 e2

      tx (EApp ef a)
        | EVar f <- dropECst ef
        , Just (dc, c)  <- L.lookup f (knConsts γ)
        , (ed, _) <- splitEAppThroughECst a
        , EVar dc' <- dropECst ed
        , dc == dc'
        = c
      tx (EIte b e1 e2)
        | isTautoPred b  = e1
        | isContraPred b = e2
      tx (ECoerc s t e)
        | s == t = e
      tx (EApp ef a)
        | EVar f <- dropECst ef
        , Just (dc, i)  <- L.lookup f (knSels γ)
        , (ed, es) <- splitEAppThroughECst a
        , EVar dc' <- dropECst ed
        , dc == dc'
        = es!!i
      tx e = e


-------------------------------------------------------------------------------
-- | Normalization of Equation: make their arguments unique -------------------
-------------------------------------------------------------------------------

class Normalizable a where
  normalize :: a -> a

instance Normalizable (GInfo c a) where
  normalize si = si {ae = normalize $ ae si}

instance Normalizable AxiomEnv where
  normalize aenv = aenv { aenvEqs   = mytracepp "aenvEqs"  (normalize <$> aenvEqs   aenv)
                        , aenvSimpl = mytracepp "aenvSimpl" (normalize <$> aenvSimpl aenv) }

instance Normalizable Rewrite where
  normalize rw = rw { smArgs = xs', smBody = normalizeBody (smName rw) $ subst su $ smBody rw }
    where
      su  = mkSubst $ zipWith (\x y -> (x,EVar y)) xs xs'
      xs  = smArgs rw
      xs' = zipWith mkSymbol xs [0 :: Integer ..]
      mkSymbol x i = x `suffixSymbol` intSymbol (smName rw) i


instance Normalizable Equation where
  normalize eq = eq {eqArgs = zip xs' ss, eqBody = normalizeBody (eqName eq) $ subst su $ eqBody eq }
    where
      su      = mkSubst $ zipWith (\x y -> (x,EVar y)) xs xs'
      (xs,ss) = unzip (eqArgs eq)
      xs'     = zipWith mkSymbol xs [0 :: Integer ..]
      mkSymbol x i = x `suffixSymbol` intSymbol (eqName eq) i

-- | Normalize the given named expression if it is recursive.
normalizeBody :: Symbol -> Expr -> Expr
normalizeBody f exprs | f `elem` syms exprs = go exprs
  where
    -- @go@ performs this simplification:
    --     (c => e1) /\ ((not c) => e2) --> if c then e1 else e2
    -- and then recurses into  e2.
    --
    -- The expressions originate from Haskell's reflect annotations, so we know
    -- that e1 is a conjunction of data constructor checkers and we do not need
    -- to recurse into e1.
    go (PAnd [PImp c e1, PImp (PNot c') e2]) | c == c' = EIte c e1 (go e2)
    go e                                               = e
normalizeBody _ e = e -- The expression is not recursive, return it unchanged.

-- -- TODO:FUEL Config
-- maxFuel :: Int
-- maxFuel = 11

-- | Increment the fuel count of the given symbol in the current evaluation
-- environment.
useFuel :: Symbol -> EvalST ()
useFuel f = do
  modify (\st -> st { evFuel = useFuelCount f (evFuel st) })

-- | Increment the fuel count.
useFuelCount :: Symbol -> FuelCount -> FuelCount
useFuelCount f fc = fc { fcMap = M.insert f (k + 1) m }
  where
    k             = M.lookupDefault 0 f m
    m             = fcMap fc

makeFreshEtaNames :: Int -> EvalST [Symbol]
makeFreshEtaNames n = replicateM n makeFreshName
  where
    makeFreshName = do
      ident <- gets freshEtaNames
      modify $ \st -> st { freshEtaNames = 1 + freshEtaNames st }
      pure $ etaExpSymbol ident

elaborateExpr :: String -> Expr -> EvalST Expr
elaborateExpr msg e = do
  let elabSpan = atLoc dummySpan msg
  env <- get
  let symEnv' = insertsSymEnv (evEnv env) (evExScope env)
  ef <- gets evElabF
  pure $ unApply $ elaborate (ElabParam ef elabSpan symEnv') e

-- | Returns False if there is a fuel count in the evaluation environment and
-- the fuel count exceeds the maximum. Returns True otherwise.
checkFuel :: Symbol -> EvalST Bool
checkFuel f = do
  fc <- gets evFuel
  case (M.lookup f (fcMap fc), fcMax fc) of
    (Just fk, Just n) -> pure (fk <= n)
    _                 -> pure True


-- Note [Existential quantification when unfolding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- After FUSION is performed, some predicates, which previously used kvars, may
-- contain existential quantifications.
--
-- When the unfoldings are searched by PLE in expressions with existentials,
-- we make sure that the produced unfoldings still have the existential
-- bindings in scope.
--
-- The procedure is as follows:
-- 1. First, we rename the existential variables in the predicates of the bindings
--    to make them unique ('renameExistentialsInSortedRefts').
--
--    @exists x y. f x y || (exists x. g x y)@
--
--    becomes
--
--    @exists v0 v1. f v0 v1 || (exists v2. g v2 v1)@
--
-- 2. We extract the nested existentials to prenex form, and we store the bodies of
--    the existentials in a map with the existential binders as keys
--    ('prenexExistentials' and 'updCtx').
--
--    @exists v0 v1. f v0 v1 || (exists v2. g v2 v1)@
--
--    produces the map
--
--    @[v0, v1, v2] -> f v0 v1 || g v2 v1@
--
-- 3. We declare to the SMT solver the existential variables in every scope
--    (in 'withAssms').
--
-- 4. We then look for unfoldings in each of the subexpressions. Whenever
--    we find an unfolding, we record the scope in which it was found.
--
--    @[v0, v1, v3] -> (f v0 v1 = v0 < v1) && (g v2 = v2 > v1)@
--
-- 5. When PLE is finished, we create for every scope an existential
--    quantification whose body contains all the corresponding unfoldings
--    and the original subexpressions in the scope ('reconstructExistentials').
--
--    @exists v0 v1 v0.
--       (f v0 v 1 = v0 < v1) && (g v2 = v2 > v1) &&
--       (f v0 v1 || g v2 v1)@
--
--    This is the expression that PLE returns.


-- | Renames existential variables in the predicates of the given bindings to
-- make them unique.
--
-- Rather than looking for all existential bindings, this function only renames
-- the superficial existentials which can be introduced by KVar solutions.
--
-- These superficial existentials appear in conjunctions, disjunctions and in the
-- body of other existentials only.
renameExistentialsInSortedRefts
  :: [(Symbol, SortedReft)]
  -> Int
  -> ([(Symbol, SortedReft)], Int)
renameExistentialsInSortedRefts binds0 existentialCounter =
    let
        binds = [ (x, sr { sr_reft = mapPredReft (const p) (sr_reft sr) }) | ((x, sr), p) <- zip binds0 preds ]
        (preds, existentialCounter') =
          renameKVarExistentials (map (reftPred . sr_reft . snd) binds0) existentialCounter
     in
        (binds, existentialCounter')

renameKVarExistentials :: [Expr] -> Int -> ([Expr], Int)
renameKVarExistentials = runState . mapM go
  where
    go (POr es) = POr <$> mapM go es
    go (PAnd es) = PAnd <$> mapM go es
    go (PExist bs e0) = do
      i1 <- get
      let i2 = i1 + length bs
      put i2
      let vs = map fst bs
          vs' = [ existSymbol v (fromIntegral i) | (v, i) <- zip vs [i1..] ]
          bs' = zip vs' (map snd bs)
          su = mkSubst $ zip vs (map EVar vs')
      PExist bs' <$> go (rapierSubstExpr (S.fromList vs') su e0)
    go e = pure e

-- ^ Scopes of existential binders identifying the location of sub-expressions
type ExScope = [(Symbol, Sort)]


-- | Extracts nested existentials from an expression.
--
-- For example, the expression
--
-- > exists [x1 : t1]. e1 == e2 &&
-- > exists [x2 : t2]. e3 == 2 &&
-- > exists [x3 : t3]. e3 < e4
--
-- would be flattened into
--
-- > (e1 == e2 && e3 == 2 && e3 < e4, [x1 : t1, x2 : t2, x3 : t3])
--
-- Precondition: the existential binding names are unique.
--
prenexExistentials :: Expr -> (ExScope, Expr)
prenexExistentials = go
  where
    go :: Expr -> (ExScope, Expr)
    go (PExist bs e) =
      let (bs', e') = go e
      in (bs ++ bs', e')
    go (PAnd es) =
      let (bss, es') = unzip (map go es)
      in (concat bss, PAnd es')
    go (POr es) =
      let (bss, es') = unzip (map go es)
      in (concat bss, POr es')
    go e = ([], e)


-- | Reconstructs expressions with existentials from a map
-- of existential scopes to their bodies.
reconstructExistentials :: M.HashMap ExScope (S.HashSet Expr) -> [Expr]
reconstructExistentials m = [ pExist s (pAndNoDedup $ S.toList es) | (s, es) <- M.toList m, not (null es) ]
