
{- | This module contains the code for the new "fast" solver, that creates
     bit-indexed propositions for each binder and constraint-id, links them
     via the environment dominator tree, after which each lhsPred is simply
     a conjunction of the RHS binder and the "current" solutions for dependent
     (cut) KVars. See [NOTE: Bit-Indexed Representation] for details.

     [NOTE: Bit-Indexed Representation]

     The whole constraint system will be represented by a collection
     of bit indexed propositions:

     Definitions:

     * Tree-Pred links are (i, i')
     * BindIds are `i`
     * SubcIds are `j`
     * KIndex  are `kI`
     * KVar    are `k`
     * Substs  are `[X := Y]` where `X` are the KVars params
     * Each BIndex = BindId i | SubcId j is represented by a
       symbol, respectively `bp[i]`, `bp[j]`
     * Solutions are `s`

      bgPred = /\_{i in BindIds} ( bp[i] <=> bindPred i  )
                           /\_{i in prev   } ( bp[i] ==> bp[prev(i)] )

      bindPred i     = p /\_{kI in kIs} kIndexPred kI
        where
          (p, kIs)       = bindExpr i

      kIndexPred kI  = * \/_{j in Js} bp[j] /\ (Y = Z[j])   IF  G[j] |- K[X:=Z[j]] ... <- kvDef k
                       * bp[kI]                             OTHERWISE
                                                            where
                                                              k[X := Y] = kvUse kI

      lhsPred s j    = /\_{kI} bp[kI] <=> kIndexSol s kI

      kIndexSol s kI = s(k) [ X:= Y ]
        where
          k[X := Y]  = kvUse kI
 -}

-- TODO: backgroundPred, lhsPred, hook into the ACTUAL SOLVER

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Language.Fixpoint.Solver.Index (

    -- * Constructor
      create

    -- * BackGround Predicate
    , bgPred

    -- * LHS Predicate
    , lhsPred

    -- DEBUG
    -- , mkDoms
    ) where

import           Prelude hiding (lookup)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types.Config
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Types                 ((&.&))
import           Language.Fixpoint.Types.Solutions
import           Language.Fixpoint.Solver.Validate (symbolSorts)
import           Language.Fixpoint.Graph            (CDeps (..))

import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
-- import qualified Data.Graph.Inductive as G
-- import qualified Data.List            as L
-- import           Data.Maybe (catMaybes)
import           Control.Monad.State

--------------------------------------------------------------------------------
-- | Creating an Index ---------------------------------------------------------
--------------------------------------------------------------------------------
create :: Config -> F.SInfo a -> [(F.KVar, Hyp)] -> CDeps -> Index
--------------------------------------------------------------------------------
create _cfg sI kHyps _cDs = FastIdx
  { bindExpr = bE
  , kvUse    = kU
  , kvDef    = kHypM
  , envBinds = F.senv <$> cm
  , envTx    = mkEnvTx kHypM bE cm
  , envSorts = F.fromListSEnv $ symbolSorts _cfg sI
  -- - , kvDeps   = {- F.tracepp "KVDeps\n" $ -} mkKvDeps kHypM bE (F.cm sI)
  -- , bindPrev = mkBindPrev sI
  }
  where
    kHypM    = M.fromList kHyps
    (bE, kU) = mkBindExpr sI
    cm       = F.cm sI

--------------------------------------------------------------------------------

mkEnvTx :: (F.KVar |-> Hyp) -> (F.BindId |-> BindPred) -> CMap (F.SimpC a) -> CMap [F.SubcId]
mkEnvTx kHyps bE = fmap S.toList . closeSubcDeps kHyps . subcDeps bE

subcDeps :: (F.BindId |-> BindPred) -> CMap (F.SimpC a) -> CMap (S.HashSet KIndex)
subcDeps bE = fmap (S.fromList . cBinds)
  where
    cBinds  = concatMap bKIs . F.elemsIBindEnv . F.senv
    bKIs i  = bpKVar $ safeLookup (msg i) i bE
    msg  i  = "subcDeps: unknown BindId " ++ show i

closeSubcDeps :: (F.KVar |-> Hyp) -> CMap (S.HashSet KIndex) -> CMap (S.HashSet F.SubcId)
closeSubcDeps kHypM kiM = cMap (execState act s0)
  where
    act = mapM_ goCI (M.keys kiM)
    s0  = DS M.empty M.empty

    goCI :: F.SubcId -> DM (S.HashSet F.SubcId)
    goCI = memoWith accCM $ \ci ->
             case M.lookup ci kiM of
               Just kis -> {- F.tracepp ("many goCI " ++ show ci) <$> -} S.insert ci <$> many goKI (S.toList kis)
               Nothing  -> error $ "goCI: unknown SubcId: " ++ show ci

    goKI  :: KIndex -> DM (S.HashSet F.SubcId)
    goKI ki
      | isNotKut ki = goK (kiKVar ki)
      | otherwise   = return S.empty

    goK  :: F.KVar -> DM (S.HashSet F.SubcId)
    goK  = memoWith accKM $ \k ->
             case M.lookup k kHypM of
               Just cs -> {- F.tracepp ("many goK " ++ show k) <$> -} many goCI (cuId <$> cs)
               _       -> error $ "goK: unknown KVar" ++ show k

    isNotKut :: KIndex -> Bool
    isNotKut = (`M.member` kHypM) . kiKVar

type DM = State DState

data DState = DS { kMap :: F.KVar   |-> S.HashSet F.SubcId
                 , cMap :: F.SubcId |-> S.HashSet F.SubcId
                 }

accKM :: Acc DM F.KVar (S.HashSet F.SubcId)
accKM = Acc (\x   -> M.lookup x . kMap <$> get)
            (\x r -> modify (\s -> s { kMap = M.insert x r (kMap s)}) >> return r)

accCM :: Acc DM F.SubcId (S.HashSet F.SubcId)
accCM = Acc (\x   -> M.lookup x . cMap <$> get)
            (\x r -> modify (\s -> s { cMap = M.insert x r (cMap s)}) >> return r)

--------------------------------------------------------------------------------

{-
mkKvDeps :: (F.KVar |-> Hyp) -> (F.BindId |-> BindPred) -> CMap (F.SimpC a) -> CMap [KIndex]
mkKvDeps kHyps bE = fmap S.toList . closeSubcDeps kHyps . subcDeps bE

subcDeps :: (F.BindId |-> BindPred) -> CMap (F.SimpC a) -> CMap (S.HashSet KIndex)
subcDeps bE = fmap (S.fromList . cBinds)
  where
    cBinds  = concatMap bKIs . F.elemsIBindEnv . F.senv
    bKIs i  = bpKVar $ safeLookup (msg i) i bE
    msg  i  = "subcDeps: unknown BindId " ++ show i

closeSubcDeps :: (F.KVar |-> Hyp) -> CMap (S.HashSet KIndex) -> CMap (S.HashSet KIndex)
closeSubcDeps kHypM kiM = cMap (execState act s0)
  where
    act = mapM_ goCI (M.keys kiM)
    s0  = DS M.empty M.empty

    goCI :: F.SubcId -> DM (S.HashSet KIndex)
    goCI = memoWith accCM $ \ci ->
             case M.lookup ci kiM of
               Just kis -> {- F.tracepp ("many goCI " ++ show ci) <$> -} many goKI (S.toList kis)
               Nothing  -> error $ "goCI: unknown SubcId: " ++ show ci

    goK  :: F.KVar -> DM (S.HashSet KIndex)
    goK  = memoWith accKM $ \k ->
             case M.lookup k kHypM of
               Just cs -> {- F.tracepp ("many goK " ++ show k) <$> -} many goCI (cuId <$> cs)
               _       -> error "impossible"

    goKI  :: KIndex -> DM (S.HashSet KIndex)
    goKI ki = case M.lookup (kiKVar ki) kHypM of
                Just _  -> {- F.tracepp ("many goKI " ++ show ki) <$> -} goK k
                Nothing -> return (S.singleton ki)
              where
                k = kiKVar ki

type DM = State DState

data DState = DS { kMap :: F.KVar   |-> S.HashSet KIndex
                 , cMap :: F.SubcId |-> S.HashSet KIndex
                 }

accKM :: Acc DM F.KVar (S.HashSet KIndex)
accKM = Acc (\x   -> M.lookup x . kMap <$> get)
            (\x r -> modify (\s -> s { kMap = M.insert x r (kMap s)}) >> return r)

accCM :: Acc DM F.SubcId (S.HashSet KIndex)
accCM = Acc (\x   -> M.lookup x . cMap <$> get)
            (\x r -> modify (\s -> s { cMap = M.insert x r (cMap s)}) >> return r)

-}

--------------------------------------------------------------------------------
mkBindExpr :: F.SInfo a -> (F.BindId |-> BindPred, KIndex |-> F.KVSub)
mkBindExpr sI = (M.fromList ips, M.fromList kSus)
  where
    kSus = [ (k, kSu)                | (_, _, iks) <- ipks, (k, kSu) <- iks    ]
    ips  = [ (i, BP p (fst <$> iks)) | (i, p, iks) <- ipks                     ]
    ipks = [ (i, p, iks)             | (i, x, r)   <- ixrs
                                     , let (p, iks) = mkBindPred i x r         ]
    ixrs = {- _checkNoDups sI $ -} F.bindEnvToList (F.bs sI)

-- TODO: we should not need the below checks once LH issue #724 is resolved.
_checkNoDups :: F.SInfo a -> [(F.BindId, F.Symbol, F.SortedReft)] -> [(F.BindId, F.Symbol, F.SortedReft)]
_checkNoDups _sI ixrs = applyNonNull ixrs dbErr bads
  where
    bads            = [(x, is) | (x, is) <- groupList xis
                               , 2 <= length is               ]
    xis             = [(x, i)  | (i, x, r) <- ixrs
                               , F.isNonTrivial r
                               -- TODO (Fix LH issue #724 and remove spl. case. on consts)
                               , not (F.memberSEnv x _consts) ]
    _consts         = F.lits _sI
    dbErr xis       = error $ "Malformed Constraints! Duplicate Binders:\n" ++ show xis


mkBindPred :: F.BindId -> F.Symbol -> F.SortedReft -> (F.Pred, [(KIndex, F.KVSub)])
mkBindPred i x sr = (F.pAnd ps, zipWith tx [0..] ks)
  where
    (ps, ks)      = F.sortedReftConcKVars x sr
    tx j k@(kv,_) = (KIndex i j kv, k)

--------------------------------------------------------------------------------
{-
mkBindPrev :: F.SInfo a -> (BIndex |-> BIndex)
mkBindPrev sI = M.fromList [(intBIndex i, intBIndex j) | (i, j) <- iDoms]
  where
    iDoms     = mkDoms bindIds cEnvs
    bindIds   = fst3   <$> F.bindEnvToList (F.bs sI)
    cEnvs     = [(i, cBinds be) | (i, be) <- M.toList (F.cm sI) ]
    cBinds    = F.elemsIBindEnv . F.senv


-- >>> mkDoms [1,2,3,4,5] [[1,2,3], [1,2,4], [1,5]]
-- [(2,1),(3,2),(4,2),(5,1)]
mkDoms :: ListNE F.BindId -> [(F.SubcId, [F.BindId])] -> [(Int, Int)]
mkDoms is envs  = G.iDom (mkEnvTree is envs) (bIndexInt Root)

mkEnvTree :: [F.BindId] -> [(F.SubcId, [F.BindId])] -> G.Gr Int ()
mkEnvTree is envs
  | isTree es   = G.mkGraph vs es
  | otherwise   = error msg
  where
    vs          = node <$> Root : (Bind <$> is)  ++ (Cstr . fst <$> envs)
    es          = edge <$> concatMap envEdges envs
    node i      = (bIndexInt i, bIndexInt i)
    edge (i, j) = (bIndexInt i, bIndexInt j, ())
    msg         = "mkBindPrev: Malformed non-tree environments!" -- TODO: move into Validate

envEdges :: (F.SubcId, [F.BindId]) -> [(BIndex, BIndex)]
envEdges (i,js) = buddies    $ [Root] ++ js' ++ [Cstr i]
  where
    js'         = Bind <$> L.sort js


-- TODO: push the `isTree` check into Validate
isTree :: (EqHash k) => [(k, k, a)] -> Bool
isTree es    = allMap (sizeLE 1) inEs
  where
    inEs     = group [ (j, i) | (i, j, _) <- es]
    sizeLE n = (<= n) . length . sortNub

buddies :: [a] -> [(a, a)]
buddies (x:y:zs) = (x, y) : buddies (y:zs)
buddies _        = []

--------------------------------------------------------------------------------
-- | Horrible hack.
--------------------------------------------------------------------------------
bIndexInt :: BIndex -> Int
bIndexInt (Root)   = 0
bIndexInt (Bind i) = i + 1
bIndexInt (Cstr j) = fromIntegral (negate (j + 1))

intBIndex :: Int -> BIndex
intBIndex i
  | 0 == i    = Root
  | 0 <  i    = Bind (i - 1)
  | otherwise = Cstr (fromIntegral (negate i) - 1)

-}

--------------------------------------------------------------------------------
-- | Encoding _all_ constraints as a single background predicate ---------------
--------------------------------------------------------------------------------
bgPred :: Index -> ([(F.Symbol, F.Sort)], F.Pred)
--------------------------------------------------------------------------------
bgPred me   = ( xts, F.PTrue )
  where
    xts     = (, F.boolSort) <$> bXs
    bXs     =  (bx <$> M.keys  (bindExpr me)) -- BindId
            ++ (bx <$> M.keys  (envBinds me)) -- SubcId

{-
    bXs     =  (bx <$> M.keys  (bindExpr me)) -- BindId
            ++ (bx <$> M.keys  (kvDeps   me)) -- SubcId
    bgPred  = F.pAnd $ [ bp i `F.PImp` bindPred me bP | (i, bP) <- iBps  ]
                     ++ [ bp i `F.PImp` bp i'          | (i, i') <- links ]
              ++ (bx       <$> M.keys   (kvUse me  )) -- KIndex
    iBps    =                M.toList (bindExpr me)
    links   = noRoots     $  M.toList (bindPrev me)
    noRoots = filter ((/= Root) . snd)

bindPred :: Index -> BindPred -> F.Pred
bindPred me (BP p kIs) = F.pAnd (p : kIps)
  where
    kIps               = kIndexPred me <$> kIs

kIndexPred :: Index -> KIndex -> F.Pred
kIndexPred me kI = case kIDef of
                     Just cs -> F.pOr (kIndexCube ySu <$> cs)
                     Nothing -> bp kI
  where
    kIDef        = M.lookup k (kvDef me)
    (k, ySu)     = safeLookup msg kI (kvUse me)
    msg          = "kIndexPred: unknown KIndex" ++ show kI

kIndexCube :: F.Subst -> Cube ->  F.Pred
kIndexCube ySu c = bp j &.& (ySu `eqSubst` zSu)
  where
    zSu          = cuSubst c
    j            = cuId    c
-}

--------------------------------------------------------------------------------
-- | `eqSubst [X := Y] [X := Z]` takes two substitutions over the SAME params
--   `X`, typically of a KVar K, and returns the equality predicate `Y = Z`
--
--------------------------------------------------------------------------------
eqSubst :: Index -> F.Subst -> F.Subst -> F.Pred
eqSubst _me (F.Su yM) (F.Su zM) = F.pAnd $ M.elems eM
  where
    eM                          = M.intersectionWith (F.PAtom F.Ueq) yM zM

{-
eqSubst _me (F.Su yM) (F.Su zM) = F.pAnd $ catMaybes $ M.elems eM
  where
    eM                          = M.intersectionWith (eqJoin _me) yM zM

eqJoin :: Index -> F.Expr -> F.Expr -> Maybe F.Pred
eqJoin me e1@(F.EVar x1) e2@(F.EVar x2)
  | all (isKnownVar me) [x1, x2]
  = Just $ F.PAtom F.Ueq e1 e2
eqJoin _   _           _
  = Nothing

isKnownVar :: Index -> F.Symbol -> Bool
isKnownVar me x = F.memberSEnv x (envSorts me)
-}

    -- [eN, yN, zN]           = M.size <$> [eM, yM, zM]
    -- msg                    = "eqSubst: incompatible substs "
    --                        ++ show yM ++ " and " ++ show zM
  -- // | eN == yN && eN == zN   =
  -- // | otherwise              = error msg


--------------------------------------------------------------------------------
-- | Flipping on bits for a single SubC, given current Solution ----------------
--------------------------------------------------------------------------------
lhsPred :: Solution -> F.SimpC a -> F.Pred
--------------------------------------------------------------------------------
lhsPred s c = F.pAnd $ [subcIdPred me nonTrivs j]
                    ++ [bp i' `F.PImp` ip'    | (i', ip') <- ips'' ]
                    ++ [bp j' `F.PImp` scp j' | j'        <- js'   ]
   where
     me          = mfromJust "Index.lhsPred" (sIdx s)
     j           = F.subcId c
     scp         = subcIdPred me nonTrivs
     nonTrivs    = M.fromList ips''
     ips''       = filter (not . F.isTautoPred . snd) ips'
     (ips', js') = footprint me s j

footprint :: Index -> Solution -> F.SubcId -> ([(F.BindId, F.Pred)], [F.SubcId])
footprint me s j = (ips', crunch jss')
  where
    crunch       = S.toList . S.unions
    (ips', jss') = unzip [ ((i', ip'), ijs')
                          | i'             <- txBindIds me j
                          , let (ijs', ip') = bindIdPred me s i' ]

txBindIds :: Index -> F.SubcId -> [F.BindId]
txBindIds me j = sortNub $ concatMap (getEnvBinds me) js
  where
    js         = j : getEnvTx me j

subcIdPred :: Index -> M.HashMap F.BindId a -> F.SubcId -> F.Pred
subcIdPred me nt j = F.pAnd (bp <$> is)
  where
    is             = filter (`M.member` nt) $ getEnvBinds me j

getEnvTx :: Index -> F.SubcId -> [F.SubcId]
getEnvTx me j = safeLookup msg j (envTx me)
  where
    msg       = "Index.getEnvTx: unknown SubcId " ++ show j

getEnvBinds :: Index -> F.SubcId -> [F.BindId]
getEnvBinds me j = F.elemsIBindEnv $ safeLookup msg j (envBinds me)
  where
    msg          = "Index.getEnvBinds: unknown SubcId " ++ show j


bindIdPred :: Index -> Solution -> F.BindId -> (S.HashSet F.SubcId, F.Pred)
bindIdPred me s i = ( S.unions js
                    , F.pAnd (p : kIPs) )
  where
    (js, kIPs)    = unzip (kIndexPred me s <$> kIs)
    BP p kIs      = safeLookup msg i (bindExpr me)
    msg           = "Index.bindIdPred: unknown BindId " ++ show i

kIndexPred :: Index -> Solution -> KIndex -> (S.HashSet F.SubcId, F.Pred)
kIndexPred me s kI = case lookup s k of
                       Right eqs -> ( mempty
                                    , qBindPred su eqs                )
                       Left  cs  -> ( S.fromList (cuId        <$> cs  )
                                    , F.pOr (kIndexCube me su <$> cs) )
  where
    (k, su)        = safeLookup msg kI (kvUse me)
    msg            = "kIndexSol: unknown KIndex" ++ show kI

kIndexCube :: Index -> F.Subst -> Cube -> F.Pred
kIndexCube me ySu c = bp j &.& eqSubst me ySu zSu
  where
    zSu             = cuSubst c
    j               = cuId    c


{-
lhsPred _ s c = {- F.tracepp ("Index.lhsPred for " ++ show j) $ -}
                  F.pAnd (bp j : [bp kI `F.PIff` kIndexSol me s kI | kI <- kIs])
  where
    me        = mfromJust "Index.lhsPred" (sIdx s)
    kIs       = safeLookup msg j (kvDeps me)
    j         = F.subcId c
    msg       = "lhsPred: unknown SubcId" ++ show j

-}



--------------------------------------------------------------------------------
-- | Helper/Local typeclass for generating bit-indices for various entities ----
--------------------------------------------------------------------------------

class BitSym a where
  bx :: a -> F.Symbol
  bp :: a -> F.Pred
  bp = F.eVar . bx

instance BitSym KIndex where
  bx (KIndex i p _) = "lq_kindex$"
                          `F.intSymbol` fromIntegral i
                          `F.intSymbol` p


instance BitSym F.BindId where
  bx = F.intSymbol "lq_bind$"

instance BitSym F.SubcId where
  bx = F.intSymbol "lq_cstr$"

instance BitSym BIndex where
  bx Root     = F.intSymbol "lq_root$" 0
  bx (Bind i) = bx i
  bx (Cstr j) = bx j

--------------------------------------------------------------------------------
-- | Tiny memoizing library used in closeSubcDeps` -----------------------------
--------------------------------------------------------------------------------

data Acc m k v = Acc {
    getF :: k -> m (Maybe v)
  , putF :: k -> v -> m v
  }

memoWith :: (Monad m) => Acc m a b -> (a -> m b) -> a -> m b
memoWith a f x = do
  ro <- getF a x
  case ro of
    Just r  -> return r
    Nothing -> putF a x =<< f x

many :: (Monad m, EqHash b) => (a -> m (S.HashSet b)) -> [a] -> m (S.HashSet b)
many f = foldM (\r x -> S.union r <$> f x) S.empty
