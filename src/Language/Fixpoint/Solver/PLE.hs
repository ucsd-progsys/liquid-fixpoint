--------------------------------------------------------------------------------
-- | This module implements "Proof by Logical Evaluation" where we
--   unfold function definitions if they *must* be unfolded, to strengthen
--   the environments with function-definition-equalities.
--   The algorithm is discussed at length in:
--
--     1. "Refinement Reflection", POPL 2018, https://arxiv.org/pdf/1711.03842
--     2. "Reasoning about Functions", VMCAI 2018, https://ranjitjhala.github.io/static/reasoning-about-functions.pdf
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wno-name-shadowing    #-}

module Language.Fixpoint.Solver.PLE
  ( instantiate

  -- The following exports are for property testing.
  , FuelCount(..)
  , ICtx(..)
  , Knowledge(..)
  , Simplifiable(..)
  )
  where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Types.Config  as FC
import           Language.Fixpoint.Types.Solutions (CMap)
import qualified Language.Fixpoint.Types.Visitor as Vis
import qualified Language.Fixpoint.Misc          as Misc
import qualified Language.Fixpoint.Smt.Interface as SMT
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
import           Language.Fixpoint.Solver.Rewrite as Rewrite

import Language.REST.OCAlgebra as OC
import Language.REST.ExploredTerms as ExploredTerms
import Language.REST.RuntimeTerm as RT
import Language.REST.SMT (withZ3, SolverHandle)

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
import           Text.PrettyPrint.HughesPJ.Compat

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

--------------------------------------------------------------------------------
-- | Strengthen Constraint Environments via PLE
--------------------------------------------------------------------------------
{-# SCC instantiate #-}
instantiate :: (Loc a) => Config -> SInfo a -> Maybe [SubcId] -> IO (SInfo a)
instantiate cfg fi' subcIds = do
    let cs = M.filterWithKey
               (\i c -> isPleCstr aEnv i c && maybe True (i `L.elem`) subcIds)
               (cm fi)
    let t  = mkCTrie (M.toList cs)                                          -- 1. BUILD the Trie
    res   <- withRESTSolver $ \solver -> withProgress (1 + M.size cs) $
               withCtx cfg file sEnv (pleTrie t . instEnv cfg fi cs solver) -- 2. TRAVERSE Trie to compute InstRes
    savePLEEqualities cfg fi res
    return $ resSInfo cfg sEnv fi res                                       -- 3. STRENGTHEN SInfo using InstRes
  where
    withRESTSolver :: (Maybe SolverHandle -> IO a) -> IO a
    withRESTSolver f | all null (M.elems $ aenvAutoRW aEnv) = f Nothing
    withRESTSolver f = withZ3 (f . Just)

    file   = srcFile cfg ++ ".evals"
    sEnv   = symbolEnv cfg fi
    aEnv   = ae fi
    fi     = normalize fi'

savePLEEqualities :: Config -> SInfo a -> InstRes -> IO ()
savePLEEqualities cfg fi res = when (save cfg) $ do
    let fq   = queryFile Files.Fq cfg ++ ".ple"
    putStrLn $ "\nSaving PLE equalities: "   ++ fq ++ "\n"
    Misc.ensurePath fq
    let constraint_equalities =
          map equalitiesPerConstraint $ Misc.hashMapToAscList $ cm fi
    writeFile fq $ render $ vcat $
      map renderConstraintRewrite constraint_equalities
  where
    equalitiesPerConstraint (cid, c) =
      (cid, L.sort [ e | i <- elemsIBindEnv (senv c), Just e <- [M.lookup i res] ])
    renderConstraintRewrite (cid, eqs) =
      "constraint id" <+> text (show cid ++ ":")
      $+$ nest 2 (toFix (pAnd eqs))
      $+$ ""

-------------------------------------------------------------------------------
-- | Step 1a: @instEnv@ sets up the incremental-PLE environment
instEnv :: (Loc a) => Config -> SInfo a -> CMap (SimpC a) -> Maybe SolverHandle -> SMT.Context -> InstEnv a
instEnv cfg fi cs restSolver ctx = InstEnv cfg ctx bEnv aEnv cs γ s0
  where
    restOC            = FC.restOC cfg
    bEnv              = bs fi
    aEnv              = ae fi
    γ                 = knowledge cfg ctx fi
    s0                = EvalEnv (SMT.ctxSymEnv ctx) mempty mempty mempty (defFuelCount cfg) et restSolver restOC
    et                = fmap makeET restSolver
    makeET solver     =
      let
        oc = ordConstraints restOC solver
      in
        ExploredTerms.empty (EF (OC.union oc) (OC.notStrongerThan oc)) ExploreWhenNeeded

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
pleTrie :: CTrie -> InstEnv a -> IO InstRes
pleTrie t env = loopT env' ctx0 diff0 Nothing res0 t
  where
    env'         = env { ieEvEnv = (ieEvEnv env) { evAccum = accum }}
    accum        = M.fromList (S.toList $ icEquals ctx0) <> evAccum (ieEvEnv env)
    diff0        = []
    res0         = M.empty
    ctx0         = initCtx ((mkEq <$> es0) ++ (mkEq' <$> es0'))
    es0          = L.filter (null . eqArgs) (aenvEqs   . ieAenv $ env)
    es0'         = L.filter (null . smArgs) (aenvSimpl . ieAenv $ env)
    mkEq  eq     = (EVar $ eqName eq, eqBody eq)
    mkEq' rw     = (EApp (EVar $ smName rw) (EVar $ smDC rw), smBody rw)

loopT
  :: InstEnv a
  -> ICtx
  -> Diff         -- ^ The longest path suffix without forks in reverse order
  -> Maybe BindId -- ^ bind id of the branch ancestor of the trie if any.
                  --   'Nothing' when this is the top-level trie.
  -> InstRes
  -> CTrie
  -> IO InstRes
loopT env ctx delta i res t = case t of
  T.Node []  -> return res
  T.Node [b] -> loopB env ctx delta i res b
  T.Node bs  -> withAssms env ctx delta Nothing $ \env' ctx' -> do
                  (ctx'', env'', res') <- ple1 env' ctx' i res
                  foldM (loopB env'' ctx'' [] i) res' bs

loopB
  :: InstEnv a
  -> ICtx
  -> Diff         -- ^ The longest path suffix without forks in reverse order
  -> Maybe BindId -- ^ bind id of the branch ancestor of the branch if any.
                  --   'Nothing' when this is a branch of the top-level trie.
  -> InstRes
  -> CBranch
  -> IO InstRes
loopB env ctx delta iMb res b = case b of
  T.Bind i t -> loopT env ctx (i:delta) (Just i) res t
  T.Val cid  -> withAssms env ctx delta (Just cid) $ \env' ctx' -> do
                  progressTick
                  (\(_, _, r) -> r) <$> ple1 env' ctx' iMb res

-- | Adds to @ctx@ candidate expressions to unfold from the bindings in @delta@
-- and the rhs of @cidMb@.
--
-- Adds to @ctx@ assumptions from @env@ and @delta@ plus rewrites that
-- candidates can use.
--
-- Sets the current constraint id in @ctx@ to @cidMb@.
--
-- Pushes assumptions from the modified context to the SMT solver, runs @act@,
-- and then pops the assumptions.
--
withAssms :: InstEnv a -> ICtx -> Diff -> Maybe SubcId -> (InstEnv a -> ICtx -> IO b) -> IO b
withAssms env@InstEnv{..} ctx delta cidMb act = do
  let (ctx', env')  = updCtx env ctx delta cidMb
  let assms = icAssms ctx'
  SMT.smtBracket ieSMT  "PLE.evaluate" $ do
    forM_ assms (SMT.smtAssert ieSMT)
    act env' ctx' { icAssms = mempty }

-- | @ple1@ performs the PLE at a single "node" in the Trie
ple1 :: InstEnv a -> ICtx -> Maybe BindId -> InstRes -> IO (ICtx, InstEnv a, InstRes)
ple1 ie@InstEnv {..} ctx i res = do
  (ctx, env) <- runStateT (evalCandsLoop ieCfg ctx ieSMT ieKnowl) ieEvEnv
  return (ctx, ie { ieEvEnv = env }, updCtxRes res i ctx)

evalToSMT :: String -> Config -> SMT.Context -> (Expr, Expr) -> Pred
evalToSMT msg cfg ctx (e1,e2) = toSMT ("evalToSMT:" ++ msg) cfg ctx [] (EEq e1 e2)

evalCandsLoop :: Config -> ICtx -> SMT.Context -> Knowledge -> EvalST ICtx
evalCandsLoop cfg ictx0 ctx γ = go ictx0 0
  where
    withRewrites exprs =
      let
        rws = [rewrite e (knSims γ) | e <- S.toList (snd `S.map` exprs)]
      in
        exprs <> S.fromList (concat rws)
    go ictx _ | S.null (icCands ictx) = return ictx
    go ictx i = do
      inconsistentEnv <- testForInconsistentEnvironment
      if inconsistentEnv
        then return ictx
        else do
                  let cands = icCands ictx
                  liftIO $ SMT.smtAssert ctx (pAndNoDedup (S.toList $ icAssms ictx))
                  let ictx' = ictx { icAssms = mempty }
                  mapM_ (evalOne γ ictx' i) (S.toList cands)
                  us <- gets evNewEqualities
                  modify $ \st -> st { evNewEqualities = mempty }
                  let unknownEqs = us `S.difference` icEquals ictx
                  if S.null unknownEqs
                        then return ictx
                        else do  let oks      = fst `S.map` unknownEqs
                                 let us'      = withRewrites unknownEqs
                                 let eqsSMT   = evalToSMT "evalCandsLoop" cfg ctx `S.map` us'
                                 let ictx''   = ictx' { icSolved = icSolved ictx <> oks
                                                      , icEquals = icEquals ictx <> us'
                                                      , icAssms  = S.filter (not . isTautoPred) eqsSMT }
                                 let newcands = mconcat (makeCandidates γ ictx'' <$> S.toList (cands <> (snd `S.map` unknownEqs)))
                                 go (ictx'' { icCands = S.fromList newcands}) (i + 1)

    testForInconsistentEnvironment =
      liftIO $ knPreds γ (knContext γ) (knLams γ) PFalse

rewrite :: Expr -> Map Symbol [Rewrite] -> [(Expr,Expr)]
rewrite e rwEnv = concatMap (`rewriteTop` rwEnv) (notGuardedApps e)

rewriteTop :: Expr -> Map Symbol [Rewrite] -> [(Expr,Expr)]
rewriteTop e rwEnv =
  [ (EApp (EVar $ smName rw) e, subst (mkSubst $ zip (smArgs rw) es) (smBody rw))
  | (EVar f, es) <- [splitEApp e]
  , Just rws <- [Map.lookup f rwEnv]
  , rw <- rws
  , length es == length (smArgs rw)
  ]

----------------------------------------------------------------------------------------------
-- | Step 3: @resSInfo@ uses incremental PLE result @InstRes@ to produce the strengthened SInfo
----------------------------------------------------------------------------------------------

resSInfo :: Config -> SymEnv -> SInfo a -> InstRes -> SInfo a
resSInfo cfg env fi res = strengthenBinds fi res'
  where
    res'     = M.fromList $ zip is ps''
    ps''     = zipWith (\i -> elaborate (atLoc dummySpan ("PLE1 " ++ show i)) env) is ps'
    ps'      = defuncAny cfg env ps
    (is, ps) = unzip (M.toList res)

----------------------------------------------------------------------------------------------
-- | @InstEnv@ has the global information needed to do PLE
----------------------------------------------------------------------------------------------

data InstEnv a = InstEnv
  { ieCfg   :: !Config
  , ieSMT   :: !SMT.Context
  , ieBEnv  :: !BindEnv
  , ieAenv  :: !AxiomEnv
  , ieCstrs :: !(CMap (SimpC a))
  , ieKnowl :: !Knowledge
  , ieEvEnv :: !EvalEnv
  }

----------------------------------------------------------------------------------------------
-- | @ICtx@ is the local information -- at each trie node -- obtained by incremental PLE
----------------------------------------------------------------------------------------------

data ICtx    = ICtx
  { icAssms    :: S.HashSet Pred            -- ^ Equalities converted to SMT format
  , icCands    :: S.HashSet Expr            -- ^ "Candidates" for unfolding
  , icEquals   :: EvAccum                   -- ^ Accumulated equalities
  , icSolved   :: S.HashSet Expr            -- ^ Terms that we have already expanded
  , icSimpl    :: !ConstMap                 -- ^ Map of expressions to constants
  , icSubcId   :: Maybe SubcId              -- ^ Current subconstraint ID
  , icANFs     :: [[(Symbol, SortedReft)]]  -- Hopefully contain only ANF things
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

initCtx :: [(Expr,Expr)] -> ICtx
initCtx es   = ICtx
  { icAssms  = mempty
  , icCands  = mempty
  , icEquals = S.fromList es
  , icSolved = mempty
  , icSimpl  = mempty
  , icSubcId = Nothing
  , icANFs   = []
  }

equalitiesPred :: S.HashSet (Expr, Expr) -> [Expr]
equalitiesPred eqs = [ EEq e1 e2 | (e1, e2) <- S.toList eqs, e1 /= e2 ]

updCtxRes :: InstRes -> Maybe BindId -> ICtx -> InstRes
updCtxRes res iMb ctx =
  updRes res iMb $ pAnd $ equalitiesPred $ icEquals ctx


updRes :: InstRes -> Maybe BindId -> Expr -> InstRes
updRes res (Just i) e = M.insertWith (error "tree-like invariant broken in ple. See https://github.com/ucsd-progsys/liquid-fixpoint/issues/496") i e res
updRes res  Nothing _ = res

----------------------------------------------------------------------------------------------
-- | @updCtx env ctx delta cidMb@ adds the assumptions and candidates from @delta@ and @cidMb@
--   to the context.
--
--  Keeps the invariant @icEquals ctx@ must be included in @evAccum (ieEvEnv env)@
----------------------------------------------------------------------------------------------

updCtx :: InstEnv a -> ICtx -> Diff -> Maybe SubcId -> (ICtx, InstEnv a)
updCtx env@InstEnv{..} ctx delta cidMb
            = ( ctx { icAssms  = S.fromList (filter (not . isTautoPred) ctxEqs)
                    , icCands  = S.fromList cands           <> icCands  ctx
                    , icEquals = initEqs                    <> icEquals ctx
                    , icSimpl  = M.fromList (S.toList sims) <> icSimpl ctx <> econsts
                    , icSubcId = cidMb
                    , icANFs   = bs : icANFs ctx
                    }
              , env { ieEvEnv = ieEvEnv { evAccum = accum } }
              )
  where
    accum     = M.fromList (S.toList initEqs) <> evAccum ieEvEnv
    initEqs   = S.fromList $ concat [rewrite e (knSims ieKnowl) | e  <- cands]
    cands     = concatMap (makeCandidates ieKnowl ctx) (rhs:es)
    sims      = S.filter (isSimplification (knDCs ieKnowl)) (initEqs <> icEquals ctx)
    econsts   = M.fromList $ findConstants ieKnowl es
    ctxEqs    = toSMT "updCtx" ieCfg ieSMT [] <$> L.nub (concat
                  [ equalitiesPred initEqs
                  , equalitiesPred sims
                  , equalitiesPred (icEquals ctx)
                  , [ expr xr   | xr@(_, r) <- bs, null (Vis.kvarsExpr $ reftPred $ sr_reft r) ]
                  ])
    bs        = second unElabSortedReft <$> binds
    rhs       = unElab eRhs
    es        = unElab <$> (expr <$> binds)
    eRhs      = maybe PTrue crhs subMb
    binds     = [ lookupBindEnv i ieBEnv | i <- delta ]
    subMb     = getCstr ieCstrs <$> cidMb


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

makeCandidates :: Knowledge -> ICtx -> Expr -> [Expr]
makeCandidates γ ctx expr
  = mytracepp ("\n" ++ show (length cands) ++ " New Candidates") cands
  where
    cands =
      filter (\e -> isRedex γ e && not (e `S.member` icSolved ctx)) (notGuardedApps expr) ++
      filter (\e -> hasConstructors γ e && not (e `S.member` icSolved ctx)) (largestApps expr)

    -- Constructor occurrences need to be considered as candidadates since
    -- they identify relevant measure equations. The function 'rewrite'
    -- introduces these equations.
    hasConstructors :: Knowledge -> Expr -> Bool
    hasConstructors γ e =  not $ S.null $ S.intersection (exprSymbolsSet e) (knDCs γ)

isRedex :: Knowledge -> Expr -> Bool
isRedex γ e = isGoodApp γ e || isIte e
  where
    isIte EIte {} = True
    isIte _       = False


isGoodApp :: Knowledge -> Expr -> Bool
isGoodApp γ e
  | (EVar f, es) <- splitEApp e
  , Just i       <- L.lookup f (knSummary γ)
  = length es >= i
  | otherwise
  = False




getCstr :: M.HashMap SubcId (SimpC a) -> SubcId -> SimpC a
getCstr env cid = Misc.safeLookup "Instantiate.getCstr" cid env

isPleCstr :: AxiomEnv -> SubcId -> SimpC a -> Bool
isPleCstr aenv sid c = isTarget c && M.lookupDefault False sid (aenvExpand aenv)

type EvAccum = S.HashSet (Expr, Expr)

--------------------------------------------------------------------------------
data EvalEnv = EvalEnv
  { evEnv      :: !SymEnv
  -- | A cache of equalities between expressions that are
  -- known to hold.
  , evAccum    :: M.HashMap Expr Expr
  , evNewEqualities :: EvAccum -- ^ Equalities discovered during a traversal of
                               -- an expression
  , evSMTCache :: M.HashMap Expr Bool -- ^ Whether an expression is valid or its negation
  , evFuel     :: FuelCount

  -- REST parameters
  , explored   :: Maybe (ExploredTerms RuntimeTerm OCType IO)
  , restSolver :: Maybe SolverHandle
  , restOCA    :: RESTOrdering
  }

data FuelCount = FC
  { fcMap :: M.HashMap Symbol Int
  , fcMax :: Maybe Int
  }
  deriving (Show)

defFuelCount :: Config -> FuelCount
defFuelCount cfg = FC mempty (fuel cfg)

type EvalST a = StateT EvalEnv IO a
--------------------------------------------------------------------------------


getAutoRws :: Knowledge -> ICtx -> [AutoRewrite]
getAutoRws γ ctx =
  Mb.fromMaybe [] $ do
    cid <- icSubcId ctx
    M.lookup cid $ knAutoRWs γ

evalOne :: Knowledge -> ICtx -> Int -> Expr -> EvalST ()
evalOne γ ctx i e
  | i > 0 || null (getAutoRws γ ctx) = void $ eval γ ctx NoRW e
evalOne γ ctx _ e = do
    env <- get
    let oc :: OCAlgebra OCType Expr IO
        oc = ordConstraints (restOCA env) (Mb.fromJust $ restSolver env)
        rp = RP oc [(e, PLE)] constraints
        constraints = OC.top oc
        emptyET = ExploredTerms.empty (EF (OC.union oc) (OC.notStrongerThan oc)) ExploreWhenNeeded
    evalREST γ ctx rp
    modify $ \st -> st { explored = Just emptyET }

-- | @notGuardedApps e@ yields all the subexpressions that are
-- applications not under an if-then-else, lambda abstraction, type abstraction,
-- type application, or quantifier.
notGuardedApps :: Expr -> [Expr]
notGuardedApps = flip go []
  where
    go e0 acc = case e0 of
      EApp e1 e2 -> e0 : go e1 (go e2 acc)
      PAnd es    -> foldr go acc es
      POr es     -> foldr go acc es
      PAtom _ e1 e2 -> go e1 $ go e2 acc
      PIff e1 e2 -> go e1 $ go e2 acc
      PImp e1 e2 -> go e1 $ go e2 acc
      EBin  _ e1 e2 -> go e1 $ go e2 acc
      PNot e -> go e acc
      ENeg e -> go e acc
      EIte b _ _ -> go b $ e0 : acc
      ECoerc _ _ e -> go e acc
      ECst e _ -> go e acc
      ESym _ -> acc
      ECon _ -> acc
      EVar _ -> acc
      ELam _ _ -> acc
      ETApp _ _ -> acc
      ETAbs _ _ -> acc
      PKVar _ _ -> acc
      PAll _ _ -> acc
      PExist _ _ -> acc
      PGrad{} -> acc

-- | @largestApps e@ yields all the largest subexpressions that are
-- applications not under an if-then-else, lambda abstraction, type abstraction,
-- type application, or quantifier.
largestApps :: Expr -> [Expr]
largestApps = flip go []
  where
    go e0 acc = case e0 of
      EApp _ _ -> e0 : acc
      PAnd es -> foldr go acc es
      POr es -> foldr go acc es
      PAtom _ e1 e2 -> go e1 $ go e2 acc
      PIff e1 e2 -> go e1 $ go e2 acc
      PImp e1 e2 -> go e1 $ go e2 acc
      EBin  _ e1 e2 -> go e1 $ go e2 acc
      PNot e -> go e acc
      ENeg e -> go e acc
      EIte b _ _ -> go b $ e0 : acc
      ECoerc _ _ e -> go e acc
      ECst e _ -> go e acc
      ESym _ -> acc
      ECon _ -> acc
      EVar _ -> e0 : acc
      ELam _ _ -> acc
      ETApp _ _ -> acc
      ETAbs _ _ -> acc
      PKVar _ _ -> acc
      PAll _ _ -> acc
      PExist _ _ -> acc
      PGrad{} -> acc


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

-- Indicates whether or not the evaluation has expanded a function statement
-- into a conditional branch.
-- In this case, rewriting should stop
-- It's unclear whether or not rewriting in either branch makes sense,
-- since one branch could be an ill-formed expression.
newtype FinalExpand = FE Bool deriving (Show)

noExpand :: FinalExpand
noExpand = FE False

expand :: FinalExpand
expand = FE True

mapFE :: (Expr -> Expr) -> (Expr, FinalExpand) -> (Expr, FinalExpand)
mapFE f (e, fe) = (f e, fe)

feVal :: FinalExpand -> Bool
feVal (FE f) = f

feAny :: [FinalExpand] -> FinalExpand
feAny xs = FE $ any feVal xs

(<|>) :: FinalExpand -> FinalExpand -> FinalExpand
(<|>) (FE True) _ = expand
(<|>) _         f = f


feSeq :: [(Expr, FinalExpand)] -> ([Expr], FinalExpand)
feSeq xs = (map fst xs, feAny (map snd xs))

-- | Unfolds expressions using rewrites and equations.
--
-- Also reduces if-then-else when the boolean condition or the negation can be
-- proved valid. This is the actual implementation of guard-validation-before-unfolding
-- that is described in publications.
--
-- Also folds constants.
--
-- Also adds to the monad state all the subexpressions that have been rewritten
-- as pairs @(original_subexpression, rewritten_subexpression)@.
--
eval :: Knowledge -> ICtx -> EvalType -> Expr -> EvalST (Expr, FinalExpand)
eval _ ctx _ e
  | Just v <- M.lookup e (icSimpl ctx)
  = return (v, noExpand)

eval γ ctx et e =
  do acc <- gets evAccum
     case M.lookup e acc of
        -- If rewriting, don't lookup, as evAccum may contain loops
        Just e' | null (getAutoRws γ ctx) -> eval γ ctx et e'
        _ -> do
          (e0', fe)  <- go e
          let e' = simplify γ ctx e0'
          if e /= e'
            then
              case et of
                NoRW -> do
                  modify (\st -> st
                    { evAccum = M.insert e e' (evAccum st)
                    , evNewEqualities = S.insert (e, e') (evNewEqualities st)
                    })
                  (e'',  fe') <- eval γ (addConst (e,e') ctx) et e'
                  return (e'', fe <|> fe')
                _ -> return (e', fe)
            else
              return (e, fe)
  where
    addConst (e,e') ctx = if isConstant (knDCs γ) e'
                           then ctx { icSimpl = M.insert e e' $ icSimpl ctx} else ctx
    go (ELam (x,s) e)   = mapFE (ELam (x, s)) <$> eval γ' ctx et e where γ' = γ { knLams = (x, s) : knLams γ }
    go (EIte b e1 e2) = evalIte γ ctx et b e1 e2
    go (ECoerc s t e)   = mapFE (ECoerc s t)  <$> go e
    go e@(EApp _ _)     =
      case splitEApp e of
       (f, es) | et == RWNormal ->
          -- Just evaluate the arguments first, to give rewriting a chance to step in
          -- if necessary
          do
            (es', fe) <- feSeq <$> mapM (eval γ ctx et) es
            if es /= es'
              then return (eApps f es', fe)
              else do
                (f', fe)  <- eval γ ctx et f
                (e', fe') <- evalApp γ ctx f' es et
                return (e', fe <|> fe')
       (f, es) ->
          do
            (f':es', fe) <- feSeq <$> mapM (eval γ ctx et) (f:es)
            (e', fe') <- evalApp γ ctx f' es' et
            return (e', fe <|> fe')

    go e@(PAtom r e1 e2) = evalBoolOr e (binOp (PAtom r) e1 e2)
    go (ENeg e)         = do (e', fe)  <- eval γ ctx et e
                             return (ENeg e', fe)
    go (EBin o e1 e2)   = do (e1', fe1) <- eval γ ctx et e1
                             (e2', fe2) <- eval γ ctx et e2
                             return (EBin o e1' e2', fe1 <|> fe2)
    go (ETApp e t)      = mapFE (`ETApp` t) <$> go e
    go (ETAbs e s)      = mapFE (`ETAbs` s) <$> go e
    go e@(PNot e')      = evalBoolOr e (mapFE PNot <$> go e')
    go e@(PImp e1 e2)   = evalBoolOr e (binOp PImp e1 e2)
    go e@(PIff e1 e2)   = evalBoolOr e (binOp PIff e1 e2)
    go e@(PAnd es)      = evalBoolOr e (efAll PAnd (go  <$$> es))
    go e@(POr es)       = evalBoolOr e (efAll POr (go <$$> es))
    go e                = return (e, noExpand)

    binOp f e1 e2 = do
      (e1', fe1) <- go e1
      (e2', fe2) <- go e2
      return (f e1' e2', fe1 <|> fe2)

    efAll f mes = do
      xs <- mes
      let (xs', fe) = feSeq xs
      return (f xs', fe)

    evalBoolOr :: Expr -> EvalST (Expr, FinalExpand) -> EvalST (Expr, FinalExpand)
    evalBoolOr ee fallback = do
      b <- evalBool γ ee
      case b of
        Just r  -> return (r, noExpand)
        Nothing -> fallback

data RESTParams oc = RP
  { oc   :: OCAlgebra oc Expr IO
  , path :: [(Expr, TermOrigin)]
  , c    :: oc
  }

-- Reverse the ANF transformation
deANF :: ICtx -> Expr -> Expr
deANF ctx = inlineInExpr (`HashMap.Lazy.lookup` undoANF id bindEnv)
  where
    bindEnv = HashMap.Lazy.unions $ map HashMap.Lazy.fromList $ icANFs ctx

-- |
-- Adds to the monad state all the subexpressions that have been rewritten
-- as pairs @(original_subexpression, rewritten_subexpression)@.
--
-- Also folds constants.
--
-- The main difference with 'eval' is that 'evalREST' takes into account
-- autorewrites.
--
evalREST :: Knowledge -> ICtx -> RESTParams OCType -> EvalST ()
evalREST γ ctx rp = do
  env <- get
  cacheRef <- liftIO $ newIORef $ evSMTCache env
  evalRESTWithCache cacheRef γ ctx rp

evalRESTWithCache
  :: IORef (M.HashMap Expr Bool) -> Knowledge -> ICtx -> RESTParams OCType -> EvalST ()
evalRESTWithCache cacheRef _ ctx rp
  | pathExprs <- map fst (mytracepp "EVAL1: path" $ path rp)
  , e         <- last pathExprs
  , Just v    <- M.lookup e (icSimpl ctx)
  = do
    smtCache <- liftIO $ readIORef cacheRef
    when (v /= e) $ modify (\st -> st
      { evAccum = M.insert e v (evAccum st)
      , evNewEqualities = S.insert (e, v) (evNewEqualities st)
      , evSMTCache = smtCache
      })

evalRESTWithCache cacheRef γ ctx rp =
  do
    Just exploredTerms <- gets explored
    se <- liftIO (shouldExploreTerm exploredTerms e)
    when se $ do
      possibleRWs <- getRWs
      rws <- notVisitedFirst exploredTerms <$> filterM (liftIO . allowed) possibleRWs
      -- liftIO $ putStrLn $ (show $ length possibleRWs) ++ " rewrites allowed at path length " ++ (show $ (map snd $ path rp))
      (e', FE fe) <- do
        r@(ec, _) <- eval γ ctx FuncNormal e
        if ec /= e
          then return r
          else eval γ ctx RWNormal e

      let evalIsNewExpr = e' `L.notElem` pathExprs
      let exprsToAdd    = [e' | evalIsNewExpr]  ++ map fst rws

      smtCache <- liftIO $ readIORef cacheRef
      modify (\st ->
                st {
                  evAccum = foldr (M.insert e) (evAccum st) exprsToAdd
                , evNewEqualities  = foldr (S.insert . (,) e) (evNewEqualities st) exprsToAdd
                , evSMTCache = smtCache
                , explored = Just $ ExploredTerms.insert
                  (Rewrite.convert e)
                  (c rp)
                  (S.insert (Rewrite.convert e') $ S.fromList (map (Rewrite.convert . fst) possibleRWs))
                  (Mb.fromJust $ explored st)
                })

      when evalIsNewExpr $
        if fe && any isRW (path rp)
          then void (eval γ (addConst (e, e')) NoRW e')
          else evalRESTWithCache cacheRef γ (addConst (e, e')) (rpEval e')

      mapM_ (evalRESTWithCache cacheRef γ ctx . rpRW) rws
  where
    shouldExploreTerm exploredTerms e =
      case rwTerminationOpts rwArgs of
        RWTerminationCheckDisabled ->
          return $ not $ ExploredTerms.visited (Rewrite.convert e) exploredTerms
        RWTerminationCheckEnabled  ->
          ExploredTerms.shouldExplore (Rewrite.convert e) (c rp) exploredTerms

    allowed (rwE, _) | rwE `elem` pathExprs = return False
    allowed (_, c)   = termCheck c
    termCheck c = Rewrite.passesTerminationCheck (oc rp) rwArgs c

    notVisitedFirst exploredTerms rws =
      let
        (v, nv) = L.partition (\(e, _) -> ExploredTerms.visited (Rewrite.convert e) exploredTerms) rws
      in
        nv ++ v

    rpEval e' =
      let
        c' =
          if any isRW (path rp)
            then refine (oc rp) (c rp) e e'
            else c rp

      in
        rp{path = path rp ++ [(e', PLE)], c = c'}

    isRW (_, r) = r == RW

    rpRW (e', c') = rp{path = path rp ++ [(e', RW)], c = c' }

    pathExprs       = map fst (mytracepp "EVAL2: path" $ path rp)
    e               = last pathExprs
    autorws         = getAutoRws γ ctx

    rwArgs = RWArgs (isValid cacheRef γ) $ knRWTerminationOpts γ

    getRWs =
      do
        -- Optimization: If we got here via rewriting, then the current constraints
        -- are satisfiable; otherwise double-check that rewriting is still allowed
        ok <-
          if isRW $ last (path rp)
            then return True
            else liftIO $ termCheck (c rp)
        if ok
          then
            do
              let e'         = deANF ctx e
              let getRW e ar = Rewrite.getRewrite (oc rp) rwArgs (c rp) e ar
              let getRWs' s  = Mb.catMaybes <$> mapM (liftIO . runMaybeT . getRW s) autorws
              concat <$> mapM getRWs' (subExprs e')
          else return []

    addConst (e,e') = if isConstant (knDCs γ) e'
                      then ctx { icSimpl = M.insert e e' $ icSimpl ctx} else ctx

(<$$>) :: (Monad m) => (a -> m b) -> [a] -> m [b]
f <$$> xs = f Misc.<$$> xs


-- | @evalApp kn ctx e es@ unfolds expressions in @eApps e es@ using rewrites
-- and equations
evalApp :: Knowledge -> ICtx -> Expr -> [Expr] -> EvalType -> EvalST (Expr, FinalExpand)
evalApp γ ctx (EVar f) es et
  | Just eq <- Map.lookup f (knAms γ)
  , length (eqArgs eq) <= length es
  = do
       env  <- gets (seSort . evEnv)
       okFuel <- checkFuel f
       if okFuel && et /= FuncNormal
         then do
                useFuel f
                let (es1,es2) = splitAt (length (eqArgs eq)) es
                shortcut (substEq env eq es1) es2 -- TODO:FUEL this is where an "unfolding" happens, CHECK/BUMP counter
         else return (eApps (EVar f) es, noExpand)
  where
    shortcut (EIte i e1 e2) es2 = do
      (b, _) <- eval γ ctx et i
      b'  <- mytracepp ("evalEIt POS " ++ showpp (i, b)) <$> isValidCached γ b
      case b' of
        Just True -> shortcut e1 es2
        Just False -> shortcut e2 es2
        _ -> return (eApps (EIte b e1 e2) es2, expand)
    shortcut e' es2 = return (eApps e' es2, noExpand)

evalApp γ _ (EVar f) (e:es) _
  | (EVar dc, as) <- splitEApp e
  , Just rws <- Map.lookup dc (knSims γ)
  , Just rw <- L.find (\rw -> smName rw == f) rws
  , length as == length (smArgs rw)
  = return (eApps (subst (mkSubst $ zip (smArgs rw) as) (smBody rw)) es, noExpand)

evalApp _ _ e es _
  = return (eApps e es, noExpand)

--------------------------------------------------------------------------------
-- | 'substEq' unfolds or instantiates an equation at a particular list of
--   argument values. We must also substitute the sort-variables that appear
--   as coercions. See tests/proof/ple1.fq
--------------------------------------------------------------------------------
substEq :: SEnv Sort -> Equation -> [Expr] -> Expr
substEq env eq es = subst su (substEqCoerce env eq es)
  where su = mkSubst $ zip (eqArgNames eq) es

substEqCoerce :: SEnv Sort -> Equation -> [Expr] -> Expr
substEqCoerce env eq es = Vis.applyCoSub coSub $ eqBody eq
  where
    ts    = snd    <$> eqArgs eq
    sp    = panicSpan "mkCoSub"
    eTs   = sortExpr sp env <$> es
    coSub = mkCoSub env eTs ts

mkCoSub :: SEnv Sort -> [Sort] -> [Sort] -> Vis.CoSub
mkCoSub env eTs xTs = M.fromList [ (x, unite ys) | (x, ys) <- Misc.groupList xys ]
  where
    unite ts    = Mb.fromMaybe (uError ts) (unifyTo1 senv ts)
    senv        = mkSearchEnv env
    uError ts   = panic ("mkCoSub: cannot build CoSub for " ++ showpp xys ++ " cannot unify " ++ showpp ts)
    xys         = Misc.sortNub $ concat $ zipWith matchSorts _xTs _eTs
    (_xTs,_eTs) = (xTs, eTs)

matchSorts :: Sort -> Sort -> [(Symbol, Sort)]
matchSorts s1 s2 = go s1 s2
  where
    go (FObj x)      {-FObj-} y    = [(x, y)]
    go (FAbs _ t1)   (FAbs _ t2)   = go t1 t2
    go (FFunc s1 t1) (FFunc s2 t2) = go s1 s2 ++ go t1 t2
    go (FApp s1 t1)  (FApp s2 t2)  = go s1 s2 ++ go t1 t2
    go _             _             = []

--------------------------------------------------------------------------------

eqArgNames :: Equation -> [Symbol]
eqArgNames = map fst . eqArgs

-- | Evaluate a boolean expression.
evalBool :: Knowledge -> Expr -> EvalST (Maybe Expr)
evalBool γ e = do
  bt <- isValidCached γ e
  case bt of
    Just True -> return $ Just PTrue
    Just False -> return $ Just PFalse
    _ -> return Nothing

isValidCached :: Knowledge -> Expr -> EvalST (Maybe Bool)
isValidCached γ e = do
  env <- get
  case M.lookup e (evSMTCache env) of
    Nothing -> do
      let isFreeInE (s, _) = not (S.member s (exprSymbolsSet e))
      b <- liftIO $ knPreds γ (knContext γ) (knLams γ) e
      if b
        then do
          when (all isFreeInE (knLams γ)) $
            put (env { evSMTCache = M.insert e True (evSMTCache env) })
          return (Just True)
        else do
          b2 <- liftIO $ knPreds γ (knContext γ) (knLams γ) (PNot e)
          if b2
            then do
              when (all isFreeInE (knLams γ)) $
                put (env { evSMTCache = M.insert e False (evSMTCache env) })
              return (Just False)
            else
              return Nothing

    mb -> return mb

-- | Evaluate @if b then e1 else e2@.
--
-- If @b@ is valid, simplifies to @e1@; if @not b@ is valid, simplifies to @e2@.
-- Otherwise the ITE is kept.
evalIte :: Knowledge -> ICtx -> EvalType -> Expr -> Expr -> Expr -> EvalST (Expr, FinalExpand)
evalIte γ ctx et b0 e1 e2 = do
  (b, fe) <- eval γ ctx et b0
  b'  <- mytracepp ("evalEIt POS " ++ showpp b) <$> isValidCached γ b
  case b' of
    Just True -> return (e1, noExpand)
    Just False -> return (e2, noExpand)
    _ -> return (EIte b e1 e2, fe)

--------------------------------------------------------------------------------
-- | Knowledge (SMT Interaction)
--------------------------------------------------------------------------------
data Knowledge = KN
  { knSims              :: Map Symbol [Rewrite]   -- ^ Rewrites rules came from match and data type definitions
                                                  --   They are grouped by the data constructor that they unfold
  , knAms               :: Map Symbol Equation -- ^ All function definitions
  , knContext           :: SMT.Context
  , knPreds             :: SMT.Context -> [(Symbol, Sort)] -> Expr -> IO Bool
  , knLams              :: ![(Symbol, Sort)]
  , knSummary           :: ![(Symbol, Int)]     -- ^ summary of functions to be evaluates (knSims and knAsms) with their arity
  , knDCs               :: !(S.HashSet Symbol)  -- ^ data constructors drawn from Rewrite
  , knSels              :: !SelectorMap
  , knConsts            :: !ConstDCMap
  , knAutoRWs           :: M.HashMap SubcId [AutoRewrite]
  , knRWTerminationOpts :: RWTerminationOpts
  }

isValid :: IORef (M.HashMap Expr Bool) -> Knowledge -> Expr -> IO Bool
isValid cacheRef γ e = do
    smtCache <- readIORef cacheRef
    case M.lookup e smtCache of
      Nothing -> do
        b <- knPreds γ (knContext γ) (knLams γ) e
        when b $
          writeIORef cacheRef (M.insert e True smtCache)
        return b
      mb -> return (mb == Just True)

knowledge :: Config -> SMT.Context -> SInfo a -> Knowledge
knowledge cfg ctx si = KN
  { knSims                     = Map.fromListWith (++) [ (smDC rw, [rw]) | rw <- sims]
  , knAms                      = Map.fromList [(eqName eq, eq) | eq <- aenvEqs aenv]
  , knContext                  = ctx
  , knPreds                    = askSMT  cfg
  , knLams                     = []
  , knSummary                  =    ((\s -> (smName s, 1)) <$> sims)
                                 ++ ((\s -> (eqName s, length (eqArgs s))) <$> aenvEqs aenv)
                                 ++ rwSyms
  , knDCs                      = S.fromList (smDC <$> sims)
  , knSels                     = Mb.mapMaybe makeSel  sims
  , knConsts                   = Mb.mapMaybe makeCons sims
  , knAutoRWs                  = aenvAutoRW aenv
  , knRWTerminationOpts        =
      if rwTerminationCheck cfg
      then RWTerminationCheckEnabled
      else RWTerminationCheckDisabled
  }
  where
    sims = aenvSimpl aenv
    aenv = ae si

    inRewrites :: Symbol -> Bool
    inRewrites e =
      let
        syms = Mb.mapMaybe (lhsHead . arLHS) (concat $ M.elems $ aenvAutoRW aenv)
      in
        e `L.elem` syms

    lhsHead :: Expr -> Maybe Symbol
    lhsHead e | (EVar f, _) <- splitEApp e = Just f
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

withCtx :: Config -> FilePath -> SymEnv -> (SMT.Context -> IO a) -> IO a
withCtx cfg file env k = do
  ctx <- SMT.makeContextWithSEnv cfg file env
  _   <- SMT.smtPush ctx
  res <- k ctx
  _   <- SMT.cleanupContext ctx
  return res


-- (sel_i, D, i), meaning sel_i (D x1 .. xn) = xi,
-- i.e., sel_i selects the ith value for the data constructor D
type SelectorMap = [(Symbol, (Symbol, Int))]
type ConstDCMap = [(Symbol, (Symbol, Expr))]

-- ValueMap maps expressions to constants (including data constructors)
type ConstMap = M.HashMap Expr Expr
type LDataCon = Symbol              -- Data Constructors

isSimplification :: S.HashSet LDataCon -> (Expr,Expr) -> Bool
isSimplification dcs (_,c) = isConstant dcs c


isConstant :: S.HashSet LDataCon -> Expr -> Bool
isConstant dcs e = S.null (S.difference (exprSymbolsSet e) dcs)

class Simplifiable a where
  simplify :: Knowledge -> ICtx -> a -> a


instance Simplifiable Expr where
  simplify γ ictx e = mytracepp ("simplification of " ++ showpp e) $ fix (Vis.mapExprOnExpr tx) e
    where
      fix f e = if e == e' then e else fix f e' where e' = f e
      tx e
        | Just e' <- M.lookup e (icSimpl ictx)
        = e'
      
      tx (PAtom rel e1 e2) = applyBooleanFolding rel e1 e2
      tx (EBin bop e1 e2) = applyConstantFolding bop e1 e2
      tx (ENeg e)         = applyConstantFolding Minus (ECon (I 0)) e
      tx (EApp e1 e2)
        | isSetPred e1    = applySetFolding e1 e2

      tx (EApp (EVar f) a)
        | Just (dc, c)  <- L.lookup f (knConsts γ)
        , (EVar dc', _) <- splitEApp a
        , dc == dc'
        = c
      tx (EIte b e1 e2)
        | isTautoPred b  = e1
        | isContraPred b = e2
      tx (ECoerc s t e)
        | s == t = e
      tx (EApp (EVar f) a)
        | Just (dc, i)  <- L.lookup f (knSels γ)
        , (EVar dc', es) <- splitEApp a
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
normalizeBody f e | f `elem` syms e = go e
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

-- | Returns False if there is a fuel count in the evaluation environment and
-- the fuel count exceeds the maximum. Returns True otherwise.
checkFuel :: Symbol -> EvalST Bool
checkFuel f = do
  fc <- gets evFuel
  case (M.lookup f (fcMap fc), fcMax fc) of
    (Just fk, Just n) -> pure (fk <= n)
    _                 -> pure True
