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
      $+$ nest 2 (vcat $ L.intersperse "" $ map (toFix . unElab) $ concatMap conjuncts eqs)
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

    -- REST Params
    et :: Maybe (ExploredTerms RuntimeTerm OCType IO)
    et                = fmap makeET restSolver
    makeET :: SolverHandle -> ExploredTerms RuntimeTerm OCType IO
    makeET solver     =
      let
        oc = ordConstraints restOC solver
      in
        ExploredTerms.empty (EF (OC.union oc) (OC.notStrongerThan oc) (OC.refine oc)) ExploreWhenNeeded

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
pleTrie t env =
    withEqualities env' (S.toList $ icEquals ctx0) $
      loopT env' ctx0 diff0 Nothing res0 t
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

withEqualities :: InstEnv a -> [(Expr, Expr)] -> IO b -> IO b
withEqualities env es m = do
  let assms = toSMT "withEqualities" (ieCfg env) (ieSMT env) [] <$> equalitiesPred es
  SMT.smtBracket (ieSMT env) "PLE.withEqualities" $ do
    forM_ assms (SMT.smtAssert (ieSMT env))
    m

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
    go ictx _ | S.null (icCands ictx) = return ictx
    go ictx i = do
      inconsistentEnv <- testForInconsistentEnvironment
      if inconsistentEnv
        then return ictx
        else do
                  liftIO $ SMT.smtAssert ctx (pAndNoDedup (S.toList $ icAssms ictx))
                  let ictx' = ictx { icAssms = mempty }
                      cands = S.toList $ icCands ictx
                  candss <- mapM (evalOne γ ictx' i) cands
                  us <- gets evNewEqualities
                  modify $ \st -> st { evNewEqualities = mempty }
                  let noCandidateChanged = and (zipWith eqCand candss cands)
                      unknownEqs = us `S.difference` icEquals ictx
                  if S.null unknownEqs && noCandidateChanged
                        then return ictx
                        else do  let eqsSMT   = evalToSMT "evalCandsLoop" cfg ctx `S.map` unknownEqs
                                 let ictx''   = ictx' { icEquals = icEquals ictx <> unknownEqs
                                                      , icAssms  = S.filter (not . isTautoPred) eqsSMT }
                                 go (ictx'' { icCands = S.fromList (concat candss) }) (i + 1)

    testForInconsistentEnvironment =
      liftIO $ knPreds γ (knContext γ) (knLams γ) PFalse

    eqCand [e0] e1 = e0 == e1
    eqCand _ _ = False

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

equalitiesPred :: [(Expr, Expr)] -> [Expr]
equalitiesPred eqs = [ EEq e1 e2 | (e1, e2) <- eqs, e1 /= e2 ]

updCtxRes :: InstRes -> Maybe BindId -> ICtx -> InstRes
updCtxRes res iMb ctx =
  updRes res iMb $ pAnd $ equalitiesPred $ S.toList $ icEquals ctx


updRes :: InstRes -> Maybe BindId -> Expr -> InstRes
updRes res (Just i) e = M.insertWith (error "tree-like invariant broken in ple. See https://github.com/ucsd-progsys/liquid-fixpoint/issues/496") i e res
updRes res  Nothing _ = res

----------------------------------------------------------------------------------------------
-- | @updCtx env ctx delta cidMb@ adds the assumptions and candidates from @delta@ and @cidMb@
--   to the context.
----------------------------------------------------------------------------------------------

updCtx :: InstEnv a -> ICtx -> Diff -> Maybe SubcId -> (ICtx, InstEnv a)
updCtx env@InstEnv{..} ctx delta cidMb
            = ( ctx { icAssms  = S.fromList (filter (not . isTautoPred) ctxEqs)
                    , icCands  = S.fromList cands           <> icCands  ctx
                    , icSimpl  = icSimpl ctx <> econsts
                    , icSubcId = cidMb
                    , icANFs   = bs : icANFs ctx
                    }
              , env { ieEvEnv = ieEvEnv { evAccum = accum } }
              )
  where
    accum     = evAccum ieEvEnv
    cands     = rhs:es
    econsts   = M.fromList $ findConstants ieKnowl es
    ctxEqs    = toSMT "updCtx" ieCfg ieSMT [] <$> L.nub
                  [ expr xr   | xr@(_, r) <- bs, null (Vis.kvarsExpr $ reftPred $ sr_reft r) ]
    bs        = second unApplySortedReft <$> binds
    rhs       = unApply eRhs
    es        = expr <$> bs
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

evalOne :: Knowledge -> ICtx -> Int -> Expr -> EvalST [Expr]
evalOne γ ctx i e
  | i > 0 || null (getAutoRws γ ctx) = (:[]) . fst <$> eval γ ctx NoRW e
evalOne γ ctx _ e = do
    env <- get
    let oc :: OCAlgebra OCType RuntimeTerm IO
        oc = ordConstraints (restOCA env) (Mb.fromJust $ restSolver env)
        rp = RP (contramap Rewrite.convert oc) [(e, PLE)] constraints
        constraints = OC.top oc
        emptyET = ExploredTerms.empty (EF (OC.union oc) (OC.notStrongerThan oc) (OC.refine oc)) ExploreWhenNeeded
    es <- evalREST γ ctx rp
    modify $ \st -> st { explored = Just emptyET }
    return es

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
          return (e', fe)
  where
    go (ELam (x,s) e)   = mapFE (ELam (x, s)) <$> eval γ' ctx et e where γ' = γ { knLams = (x, s) : knLams γ }
    go (EIte b e1 e2) = evalIte γ ctx et b e1 e2
    go (ECoerc s t e)   = mapFE (ECoerc s t)  <$> go e
    go e@(EApp _ _)     =
      case splitEAppThroughECst e of
       (f, es) | et == RWNormal ->
          -- Just evaluate the arguments first, to give rewriting a chance to step in
          -- if necessary
          do
            (es', fe) <- feSeq <$> mapM (eval γ ctx et) es
            if es /= es'
              then return (eApps f es', fe)
              else do
                (f', fe)  <- eval γ ctx et f
                (me', fe') <- evalApp γ ctx f' es et
                return (Mb.fromMaybe (eApps f' es') me', fe <|> fe')
       (f, es) ->
          do
            (f':es', fe) <- feSeq <$> mapM (eval γ ctx et) (f:es)
            (me', fe') <- evalApp γ ctx f' es' et
            return (Mb.fromMaybe (eApps f' es') me', fe <|> fe')

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
    go (ECst e t)       = do (e', fe) <- eval γ ctx et e
                             return (ECst e' t, fe)
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
      { evAccum = M.insert e v (evAccum st)
      , evNewEqualities = S.insert (e, v) (evNewEqualities st)
      , evSMTCache = smtCache
      })
    return (v : acc)

evalRESTWithCache cacheRef γ ctx acc rp =
  do
    Just exploredTerms <- gets explored
    se <- liftIO (shouldExploreTerm exploredTerms e)
    if se then do
      possibleRWs <- getRWs
      rws <- notVisitedFirst exploredTerms <$> filterM (liftIO . allowed) possibleRWs
      -- liftIO $ putStrLn $ (show $ length possibleRWs) ++ " rewrites allowed at path length " ++ (show $ (map snd $ path rp))
      (e', FE fe) <- do
        r@(ec, _) <- eval γ ctx FuncNormal e
        if ec /= e
          then return r
          else eval γ ctx RWNormal e

      let evalIsNewExpr = e' `L.notElem` pathExprs
      let exprsToAdd    = [e' | evalIsNewExpr]  ++ map (\(_, e, _) -> e) rws
          acc' = exprsToAdd ++ acc
          eqnToAdd = map (\(eqn, _, _) -> eqn) rws

      smtCache <- liftIO $ readIORef cacheRef
      modify (\st ->
            let evAccum1 = foldr (M.insert e) (evAccum st) exprsToAdd
             in st {
                  evAccum = foldr (uncurry M.insert) evAccum1 eqnToAdd
                , evNewEqualities  = foldr S.insert (evNewEqualities st) eqnToAdd
                , evSMTCache = smtCache
                , explored = Just $ ExploredTerms.insert
                  (Rewrite.convert e)
                  (c rp)
                  (S.insert (Rewrite.convert e') $ S.fromList (map (Rewrite.convert . (\(_, e, _) -> e)) possibleRWs))
                  (Mb.fromJust $ explored st)
                })

      acc'' <- if evalIsNewExpr
        then if fe && any isRW (path rp)
          then (:[]) . fst <$> eval γ (addConst (e, e')) NoRW e'
          else evalRESTWithCache cacheRef γ (addConst (e, e')) acc' (rpEval e')
        else return acc'

      foldM (\r rw -> evalRESTWithCache cacheRef γ ctx r (rpRW rw)) acc'' rws
     else
      return acc
  where
    shouldExploreTerm exploredTerms e =
      case rwTerminationOpts rwArgs of
        RWTerminationCheckDisabled ->
          return $ not $ ExploredTerms.visited (Rewrite.convert e) exploredTerms
        RWTerminationCheckEnabled  ->
          ExploredTerms.shouldExplore (Rewrite.convert e) (c rp) exploredTerms

    allowed (_, rwE, _) | rwE `elem` pathExprs = return False
    allowed (_, _, c)   = termCheck c
    termCheck c = Rewrite.passesTerminationCheck (oc rp) rwArgs c

    notVisitedFirst exploredTerms rws =
      let
        (v, nv) = L.partition (\(_, e, _) -> ExploredTerms.visited (Rewrite.convert e) exploredTerms) rws
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

    rpRW (_, e', c') = rp{path = path rp ++ [(e', RW)], c = c' }

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
evalApp :: Knowledge -> ICtx -> Expr -> [Expr] -> EvalType -> EvalST (Maybe Expr, FinalExpand)
evalApp γ ctx e0 es et
  | EVar f <- dropECst e0
  , Just eq <- Map.lookup f (knAms γ)
  , length (eqArgs eq) <= length es
  = do
       env  <- gets (seSort . evEnv)
       okFuel <- checkFuel f
       if okFuel && et /= FuncNormal
         then do
                useFuel f
                let (es1,es2) = splitAt (length (eqArgs eq)) es
                    newE = substEq env eq es1
                (e', fe) <- shortcut γ ctx et newE es2 -- TODO:FUEL this is where an "unfolding" happens, CHECK/BUMP counter
                modify $ \st ->
                  st { evNewEqualities = S.insert (eApps e0 es, e') (evNewEqualities st) }
                return (Just e', fe)
         else return (Nothing, noExpand)

evalApp γ _ e0 args@(e:es) _
  | EVar f <- dropECst e0
  , (d, as) <- splitEAppThroughECst e
  , EVar dc <- dropECst d
  , Just rws <- Map.lookup dc (knSims γ)
  , Just (rw, isUserDataSMeasure) <- L.find (\(rw, _) -> smName rw == f) rws
  , length as == length (smArgs rw)
  = do
    let newE = eApps (subst (mkSubst $ zip (smArgs rw) as) (smBody rw)) es
        measureEqs = nonUserDataMeasureEqs γ e
        eqs = if isUserDataSMeasure == NoUserDataSMeasure
                -- User data measures aren't sent to the SMT solver because
                -- it knows already about selectors and constructor tests.
                then (eApps e0 args, newE) : measureEqs
                else measureEqs
    modify $ \st ->
      st { evNewEqualities = foldr S.insert (evNewEqualities st) eqs }
    return (Just newE, noExpand)

evalApp γ ctx e0 (e1:es2) et
  | EVar f <- dropECst e0
  , Just (rws, NoUserDataSMeasure) <- Map.lookup f (knSimsByMeasureName γ)
  = do
       okFuel <- checkFuel f
       if okFuel && et /= FuncNormal
         then do
                useFuel f
                let newE = substRW γ rws e1
                (e', fe) <- shortcut γ ctx et newE es2 -- TODO:FUEL this is where an "unfolding" happens, CHECK/BUMP counter
                modify $ \st ->
                  st { evNewEqualities = S.insert (eApps e0 (e1:es2), e') (evNewEqualities st) }
                return (Just e', fe)
         else return (Nothing, noExpand)

evalApp γ _ctx e0 es _et
  | eqs@(_:_) <- nonUserDataMeasureEqs γ (eApps e0 es)
  = do
       modify $ \st ->
         st { evNewEqualities = foldr S.insert (evNewEqualities st) eqs }
       return (Nothing, noExpand)

evalApp _ _ _e _es _
  = return (Nothing, noExpand)

shortcut :: Knowledge -> ICtx -> EvalType -> Expr -> [Expr] -> EvalST (Expr, FinalExpand)
shortcut γ ctx et (EIte i e1 e2) es2 = do
      (b, _) <- eval γ ctx et i
      b'  <- mytracepp ("evalEIt POS " ++ showpp (i, b)) <$> isValidCached γ b
      case b' of
        Just True -> shortcut γ ctx et e1 es2
        Just False -> shortcut γ ctx et e2 es2
        _ -> return (eApps (EIte b e1 e2) es2, expand)
shortcut _ _ _ e' es2 = return (eApps e' es2, noExpand)

-- | unfolds a measure when the argument is not a known
-- constructor
substRW :: Knowledge -> [Rewrite] -> Expr -> Expr
substRW γ rws0 e1 = go rws0
  where
    go [rw] = mkBranch rw
    go (rw:rws) = EIte (EApp (testConstructor (smDC rw)) e1) (mkBranch rw) (go rws)
    go [] = error "substRW: Unexpected empty rewrites"

    mkBranch rw =
      let mdc = M.lookup (smDC rw) (knDataCtors γ)
          su = [ (s, mkSelector mdc (smDC rw) i e1) | (i, s) <- zip [0..] (smArgs rw) ]
       in subst (mkSubst su) (smBody rw)

    mkSelector mdc dcname i =
      case mdc of
        Just dc -> EApp (EVar $ val $ dfName $ dcFields dc !! i)
        Nothing -> case L.find (\(_, (dc, si)) -> i == si && dc == dcname) (knSels γ) of
          Just (selname, _) -> EApp (EVar selname)
          Nothing -> error $ "substRW: can't find selector for " ++ show dcname
    testConstructor = EVar . testSymbol

-- | Creates equations that explain how to rewrite a given constructor
-- application with all non-user data measures
nonUserDataMeasureEqs :: Knowledge -> Expr -> [(Expr,Expr)]
nonUserDataMeasureEqs γ e =
  [ (EApp (EVar $ smName rw) e, subst (mkSubst $ zip (smArgs rw) es) (smBody rw))
  | (ef, es) <- [splitEAppThroughECst e]
  , EVar f <- [dropECst ef]
  , Just rws <- [Map.lookup f (knNonUserDataMeasures γ)]
  , rw <- rws
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
substEqCoerce env eq es = Vis.applyCoSub coSub $ eqBody eq
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
mkCoSub :: SEnv Sort -> [Sort] -> [Sort] -> Vis.CoSub
mkCoSub env eTs xTs = M.fromList [ (x, unite ys) | (x, ys) <- Misc.groupList xys ]
  where
    unite ts    = Mb.fromMaybe (uError ts) (unifyTo1 senv ts)
    senv        = mkSearchEnv env
    uError ts   = panic ("mkCoSub: cannot build CoSub for " ++ showpp xys ++ " cannot unify " ++ showpp ts)
    xys         = Misc.sortNub $ concat $ zipWith matchSorts xTs eTs

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
  { -- | Rewrites rules came from match definitions
    --
    -- They are grouped by the data constructor that they unfold, and are
    -- augmented with an attribute that say whether they originate from a
    -- user data declaration.
    knSims              :: Map Symbol [(Rewrite, IsUserDataSMeasure)]
    -- | Like 'knSims' but rewrites are grouped by measure name, and this
    -- does not include non-user-defined data measures
  , knSimsByMeasureName :: Map Symbol ([Rewrite], IsUserDataSMeasure)
    -- | Non-user-defined data measures. e.g. null, head, and tail
  , knNonUserDataMeasures :: Map Symbol [Rewrite]
  , knAms               :: Map Symbol Equation -- ^ All function definitions
  , knContext           :: SMT.Context
  , knPreds             :: SMT.Context -> [(Symbol, Sort)] -> Expr -> IO Bool
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
  { knSims                     = Map.fromListWith (++) $
                                   [ (smDC rw, [(rw, NoUserDataSMeasure)]) | rw <- sims ] ++
                                   [ (smDC rw, [(rw, UserDataSMeasure)]) | rw <- dataSims ]
  , knSimsByMeasureName        = Map.fromListWith (\(rw0, dms) (rw1, _) -> (rw0 ++ rw1, dms)) $
                                   [ (smName rw, ([rw], NoUserDataSMeasure))
                                   | rw <- sims
                                   , not (isTestSymbol (smName rw)) -- not a constructor test
                                   , not (isSelector rw)
                                   ] ++
                                   [ (smName rw, ([rw], UserDataSMeasure)) | rw <- dataSims ]
  , knNonUserDataMeasures      = Map.fromListWith (++) [ (smDC rw, [rw])
                                   | rw <- sims
                                   , isTestSymbol (smName rw) -- a constructor test
                                     || isSelector rw
                                   ]
  , knAms                      = Map.fromList [(eqName eq, eq) | eq <- aenvEqs aenv]
  , knContext                  = ctx
  , knPreds                    = askSMT  cfg
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
      if rwTerminationCheck cfg
      then RWTerminationCheckEnabled
      else RWTerminationCheckDisabled
  }
  where
    isEVar (EVar _) = True
    isEVar _ = False

    isSelector rw = isEVar (smBody rw) && elem (smBody rw) (map EVar (smArgs rw))

    (simDCTests, sims0) =
      partitionUserDataConstructorTests (ddecls si) $ aenvSimpl aenv
    (simDCSelectors, sims) =
      partitionUserDataConstructorSelectors (ddecls si) sims0
    dataSims = simDCTests ++ simDCSelectors
    aenv = ae si

    inRewrites :: Symbol -> Bool
    inRewrites e =
      let
        syms = Mb.mapMaybe (lhsHead . arLHS) (concat $ M.elems $ aenvAutoRW aenv)
      in
        e `L.elem` syms

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
