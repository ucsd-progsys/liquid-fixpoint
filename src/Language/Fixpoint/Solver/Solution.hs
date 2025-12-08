{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Fixpoint.Solver.Solution
  ( -- * Create Initial Solution
    init

    -- * Update Solution
  , Sol.update

    -- * Apply Solution
  , applyInSortedReft
  , CombinedEnv(..)

    -- * Lookup Solution
  , lhsPred

  , nonCutsResult
  ) where

import           Control.Arrow (second, (***))
import           Control.Monad                  (guard)
import           Control.Monad.Reader
import qualified Data.HashSet                   as S
import qualified Data.HashMap.Strict            as M
import           Data.Maybe                     (maybeToList, isJust, isNothing)
import           Language.Fixpoint.Types.PrettyPrint ()
import           Language.Fixpoint.Types.Visitor      as V
import           Language.Fixpoint.SortCheck          (ElabM)
import qualified Language.Fixpoint.SortCheck          as So
import qualified Language.Fixpoint.Misc               as Misc
import           Language.Fixpoint.Types.Config
import qualified Language.Fixpoint.Types              as F
import qualified Language.Fixpoint.Types.Solutions    as Sol
import           Language.Fixpoint.Types.Constraints  hiding (ws, bs)
import           Prelude                              hiding (init, lookup)


--------------------------------------------------------------------------------
-- | Initial Solution (from Qualifiers and WF constraints) ---------------------
--------------------------------------------------------------------------------
init :: (F.Fixpoint a) => Config -> F.SInfo a -> S.HashSet F.KVar -> M.HashMap F.KVar Sol.QBind
--------------------------------------------------------------------------------
init cfg si ks =
    runReader (traverse (refine si qcs genv) ws) (solverFlags $ solver cfg)
  where
    qcs = mkQCluster (F.quals si)
    ws = M.intersection (F.ws si) (S.toMap ks)
    genv = initQualifierEnv cfg si

initQualifierEnv :: (F.Fixpoint a) => Config -> F.SInfo a -> F.SEnv F.Sort
initQualifierEnv cfg si
  | scraping  = So.globalEnv cfg si <> instConstants si
  | otherwise = instConstants si
  where
    scraping = scrape cfg /= No

--------------------------------------------------------------------------------
-- | [NOTE:qual-cluster] It is wasteful to perform instantiation *individually*
--   on each qualifier, as many qualifiers have "equivalent" parameters, and
--   so have the "same" instances in an environment. To exploit this structure,
--
--   1. Group the [Qualifier] into a QCluster
--   2. Refactor instK to use QCluster
--------------------------------------------------------------------------------

type QCluster = M.HashMap QCSig [Qualifier]

type QCSig = [F.QualParam]

mkQCluster :: [Qualifier] -> QCluster
mkQCluster = Misc.groupMap qualSig

qualSig :: Qualifier -> QCSig
qualSig q = [ p { F.qpSym = F.dummyName }  | p <- F.qParams q ]

--------------------------------------------------------------------------------

refine :: F.SInfo a -> QCluster -> F.SEnv F.Sort -> F.WfC a -> ElabM Sol.QBind
refine info qs genv w = refineK (allowHOquals info) env qs (F.wrft w)
  where
    env             = wenvSort <> genv
    wenvSort        = F.sr_sort <$> F.fromListSEnv (F.envCs (F.bs info) (F.wenv w))

instConstants :: F.SInfo a -> F.SEnv F.Sort
instConstants = F.fromListSEnv . filter notLit . F.toListSEnv . F.gLits
  where
    notLit    = not . F.isLitSymbol . fst


refineK :: Bool -> F.SEnv F.Sort -> QCluster -> (F.Symbol, F.Sort, F.KVar) -> ElabM Sol.QBind
refineK ho env qs (v, t, _k) = Sol.qbFilterM (okInst env v t) eqs
   where
    eqs = instK ho env v t qs

--------------------------------------------------------------------------------
instK :: Bool
      -> F.SEnv F.Sort
      -> F.Symbol
      -> F.Sort
      -> QCluster
      -> Sol.QBind
--------------------------------------------------------------------------------
instK ho env v t qc = Sol.qb . unique $
  [ Sol.eQual q xs
      | (sig, qs) <- M.toList qc
      , xs        <- instKSig ho env v t sig
      , q         <- qs
  ]

unique :: [Sol.EQual] -> [Sol.EQual]
unique qs = M.elems $ M.fromList [ (Sol.eqPred q, q) | q <- qs ]

instKSig :: Bool
         -> F.SEnv F.Sort
         -> F.Symbol
         -> F.Sort
         -> QCSig
         -> [[F.Symbol]]
instKSig _  _   _ _ [] = error "Empty qsig in Solution.instKSig"
instKSig ho env v sort' (qp:qps) = do
  (su0, i0, qs0) <- candidatesP symToSrch [(0, sort', [v])] qp
  ixs       <- matchP symToSrch tyss [(i0, qs0)] (applyQPP su0 <$> qps)
  ys        <- instSymbol tyss (tail $ reverse ixs)
  return (v:ys)
  where
    tyss       = zipWith (\i (t, ys) -> (i, t, ys)) [1..] (instCands ho env)
    symToSrch  = (`F.lookupSEnvWithDistance` env)

instSymbol :: [(SortIdx, a, [F.Symbol])] -> [(SortIdx, QualPattern)] -> [[F.Symbol]]
instSymbol tyss = go
  where
    m = M.fromList [(i, ys) | (i,_,ys) <- tyss]
    go [] =
      return []
    go ((i,qp):is) = do
      y   <- M.lookupDefault [] i m
      qsu <- maybeToList (matchSym qp y)
      ys  <- go [ (i', applyQPSubst qsu  qp') | (i', qp') <- is]
      return (y:ys)

instCands :: Bool -> F.SEnv F.Sort -> [(F.Sort, [F.Symbol])]
instCands ho env = filter isOk tyss
  where
    tyss      = Misc.groupList [(t, x) | (x, t) <- xts]
    isOk      = if ho then const True else isNothing . F.functionSort . fst
    xts       = F.toListSEnv env


type SortIdx = Int

matchP :: So.Env -> [(SortIdx, F.Sort, a)] -> [(SortIdx, QualPattern)] -> [F.QualParam] ->
          [[(SortIdx, QualPattern)]]
matchP env tyss = go
  where
    go' !i !p !is !qps  = go ((i, p):is) qps
    go is (qp : qps) = do (su, i, pat) <- candidatesP env tyss qp
                          go' i pat is (applyQPP su <$> qps)
    go is []         = return is

applyQPP :: So.TVSubst -> F.QualParam -> F.QualParam
applyQPP su qp = qp
  { qpSort = So.apply     su  (qpSort qp)
  }

-- match :: So.Env -> [(F.Sort, [F.Symbol])] -> [F.Symbol] -> [F.QualParam] -> [[F.Symbol]]
-- match env tyss xs (qp : qps)
--   = do (su, qsu, x) <- candidates env tyss qp
--        match env tyss (x : xs) (applyQP su qsu <$> qps)
-- match _   _   xs []
--   = return xs

-- applyQP :: So.TVSubst -> QPSubst -> F.QualParam -> F.QualParam
-- applyQP su qsu qp = qp
--   { qpSort = So.apply     su  (qpSort qp)
--   , qpPat  = applyQPSubst qsu (qpPat qp)
--   }

--------------------------------------------------------------------------------
candidatesP :: So.Env -> [(SortIdx, F.Sort, a)] -> F.QualParam ->
               [(So.TVSubst, SortIdx, QualPattern)]
--------------------------------------------------------------------------------
candidatesP env tyss x =
    [(su, idx, qPat)
        | (idx, t,_)  <- tyss
        , su          <- maybeToList (So.unifyFast mono env xt t)
    ]
  where
    xt   = F.qpSort x
    qPat = F.qpPat  x
    mono = So.isMono xt

-- --------------------------------------------------------------------------------
-- candidates :: So.Env -> [(F.Sort, [F.Symbol])] -> F.QualParam
--            -> [(So.TVSubst, QPSubst, F.Symbol)]
-- --------------------------------------------------------------------------------
-- candidates env tyss x = -- traceShow _msg
--     [(su, qsu, y) | (t, ys)  <- tyss
--                   , su       <- maybeToList (So.unifyFast mono env xt t)
--                   , y        <- ys
--                   , qsu      <- maybeToList (matchSym x y)
--     ]
--   where
--     xt   = F.qpSort x
--     mono = So.isMono xt
--     _msg = "candidates tyss :=" ++ F.showpp tyss ++ "tx := " ++ F.showpp xt

matchSym :: F.QualPattern -> F.Symbol -> Maybe QPSubst
matchSym qp y' = case qp of
  F.PatPrefix s i -> JustSub i <$> F.stripPrefix s y
  F.PatSuffix i s -> JustSub i <$> F.stripSuffix s y
  F.PatNone       -> Just NoSub
  F.PatExact s    -> if s == y then Just NoSub else Nothing
  where
    y             =  F.unKArgSymbol y'

data QPSubst = NoSub | JustSub Int F.Symbol

applyQPSubst :: QPSubst -> F.QualPattern -> F.QualPattern
applyQPSubst (JustSub i x) (F.PatPrefix s j)
  | i == j = F.PatExact (F.mappendSym s x)
applyQPSubst (JustSub i x) (F.PatSuffix j s)
  | i == j = F.PatExact (F.mappendSym x s)
applyQPSubst _ p
  = p

--------------------------------------------------------------------------------
okInst :: F.SEnv F.Sort -> F.Symbol -> F.Sort -> Sol.EQual -> ElabM Bool
--------------------------------------------------------------------------------
okInst env v t eq =
  do tc <- So.checkSorted (F.srcSpan eq) env sr
     pure $ isNothing tc
  where
    sr            = F.RR t (F.Reft (v, p))
    p             = Sol.eqPred eq

    -- _msg          = printf "okInst: t = %s, eq = %s, env = %s" (F.showpp t) (F.showpp eq) (F.showpp env)


--------------------------------------------------------------------------------
-- | Predicate corresponding to LHS of constraint in current solution
--------------------------------------------------------------------------------
{-# SCC lhsPred #-}
lhsPred
  :: (F.Loc a)
  => F.IBindEnv
  -> F.BindEnv a
  -> Sol.Solution
  -> F.SimpC a
  -> F.Expr
lhsPred bindingsInSmt be s c =
    let ap = apply g s bs
     in F.notracepp _msg $ fst ap
  where
    g          = CEnv ci be bs (F.srcSpan c) bindingsInSmt
    bs         = F.senv c
    ci         = sid c
    _msg       = "LhsPred for id = " ++ show (sid c) ++ " with SOLUTION = " ++ F.showpp s

data CombinedEnv a = CEnv
  { ceCid  :: !Cid
  , ceBEnv :: !(F.BindEnv a)
  , ceIEnv :: !F.IBindEnv
  , ceSpan :: !F.SrcSpan
    -- | These are the bindings that the smt solver knows about and can be
    -- referred as @EVar (bindSymbol <bindId>)@ instead of serializing them
    -- again.
  , ceBindingsInSmt :: !F.IBindEnv
  }

type Cid         = Maybe Integer
type ExprInfo    = (F.Expr, KInfo)

apply :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.IBindEnv -> ExprInfo
apply g s bs =
    -- Clear the "known" bindings for applyKVars, since it depends on
    -- using the fully expanded representation of the predicates to bind their
    -- variables with quantifiers.
    let xrs = map (lookupBindEnvExt g) (F.elemsIBindEnv bs)
        (ps,  ks) = envConcKVars xrs
        (pks, kI) = applyKVars g {ceBindingsInSmt = F.emptyIBindEnv} s ks
     in (F.conj (pks:ps), kI)   -- see [NOTE: pAnd-SLOW]

-- | @applyInSortedReft@ applies the solution to a single sorted reft
applyInSortedReft
  :: CombinedEnv ann
  -> Sol.Sol Sol.QBind
  -> (F.Symbol, F.SortedReft)
  -> (F.Symbol, F.SortedReft)
applyInSortedReft g s xsr@(x, sr) =
    let (ps,  ks) = envConcKVars [xsr]
        (pks, _) = applyKVars g {ceBindingsInSmt = F.emptyIBindEnv} s ks
     in (x, sr { F.sr_reft = F.Reft (x, F.conj (pks : ps)) })

-- | Produces conjuncts of each sorted reft in the IBindEnv, separated
-- into concrete conjuncts and kvars.
envConcKVars :: [(F.Symbol, F.SortedReft)] -> ([F.Expr], [F.KVSub])
envConcKVars xrs =
  let (pss, kss) = unzip [ F.sortedReftConcKVars x sr | (x, sr) <- xrs ]
   in (concat pss, concat kss)

lookupBindEnvExt
  :: CombinedEnv ann -> F.BindId -> (F.Symbol, F.SortedReft)
lookupBindEnvExt g i =
     (,) x $ 
       if F.memberIBindEnv i (ceBindingsInSmt g)
       then sr { F.sr_reft = F.Reft (x, F.EVar (F.bindSymbol (fromIntegral i)))}
       else sr
   where
      (x, sr, _)              = F.lookupBindEnv i (ceBEnv g)

applyKVars :: CombinedEnv ann -> Sol.Sol Sol.QBind -> [F.KVSub] -> ExprInfo
applyKVars g s ks =
  let bcs = map (applyKVar g s) ks
      (es, is) = unzip bcs
   in (F.pAndNoDedup es, mconcat is)

applyKVar :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> ExprInfo
applyKVar g s ksu = case Sol.lookup s (F.ksuKVar ksu) of
  Left cs   -> hypPred g s ksu cs
  Right eqs -> let qbp = Sol.qbPreds (F.ksuSubst ksu) eqs
                in (F.pAndNoDedup $ fst <$> qbp, mempty) -- TODO: don't initialize kvars that have a hyp solution

mkNonCutsExpr :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVar -> Sol.Hyp -> F.Expr
mkNonCutsExpr ce s k cs =
  let bcps = map (bareCubePred ce s k) cs
   in F.pOr bcps

nonCutsResult :: F.BindEnv ann -> Sol.Sol Sol.QBind -> FixDelayedSolution
nonCutsResult be s = M.mapWithKey (\k -> Delayed . mkNonCutsExpr g s k) $ Sol.sHyp s
  where
    g = CEnv Nothing be F.emptyIBindEnv F.dummySpan F.emptyIBindEnv


-- | Produces a predicate from a constraint defining a kvar.
--
-- This is written in imitation of 'cubePred'. However, there are some
-- differences since the result of 'cubePred' is fed to the verification
-- pipeline and @bareCubePred@ is meant for human inspection.
--
-- The expression is created from its defining constraints only, while
-- @cubePred@ does expect the caller to supply the substitution at a
-- particular use of the KVar. Thus @cubePred@ produces a different
-- expression for every use site of the kvar, while here we produce one
-- expression for all the uses.
--
-- Where the cube rhs is @k[params:=xts]@, we keep the parameters free in the
-- final predicate. e.g. @params == xts && exists yts . ...@
-- That is, we only quantify out the `yts` as we want to make
-- explicit what equalities those parameters have in each cube.
--
-- Issue https://github.com/ucsd-progsys/liquid-fixpoint/issues/808 discusses
-- an example where the equalities are essential to keep.

bareCubePred :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVar -> Sol.Cube -> F.Expr
bareCubePred g s k c =
    let psu = F.pAnd [ F.EEq (F.expr x) e | (x, e) <- M.toList m ]
        (p, _kI) = apply g' s bs'
     in F.pExist yts (p F.&.& psu)
  where
    bs = Sol.cuBinds c
    F.Su m = dropUnsortedExprs g' (Sol.cuSubst c)
    g' = addCEnv  g bs
    bs' = F.diffIBindEnv bs (Misc.safeLookup "sScp" k (Sol.sScp s))
    yts = symSorts g bs'

-- | At the moment, the liquid-fixpoint implementation allows for unsorted
-- expressions in substitutions. See the discussion in
-- https://github.com/ucsd-progsys/liquid-fixpoint/issues/800
dropUnsortedExprs :: CombinedEnv ann -> F.Subst -> F.Subst
dropUnsortedExprs g (F.Su m) = F.Su $
    M.filter
      (\e -> isJust $ do
         t <- So.checkSortExpr sp env e
         guard (not (isClass t))
      )
      m
  where
    sp  = ceSpan g
    env = combinedSEnv g

hypPred :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> Sol.Hyp -> ExprInfo
hypPred g s ksu hyp =
  let cs = map (cubePred g s ksu) hyp
   in F.pOr *** mconcatPlus $ unzip cs

{- | `cubePred g s k su c` returns the predicate for

        (k . su)

      defined by using cube

        c := [b1,...,bn] |- (k . su')

      in the binder environment `g`. The binders in `sScp s k` are not included
      in the final predicate. They are considered redundant conjuncts as per
      section 2.4 of "Local Refinement Typing", ICFP 2017.
 -}
cubePred :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> Sol.Cube -> ExprInfo
cubePred g s ksu c    =
    let (p, kI) = cubePredExc g s c bs'
        -- Free variables in p should not colide with those generated by
        -- the rapier substitution. If that were the case, perhaps we would
        -- need to include @combinedSEnv g@ in the scope set.
     in (F.rapierSubstExpr (F.substSymbolsSet su) su p, kI)
  where
    bs' = F.diffIBindEnv bs (Misc.safeLookup "sScp" k (Sol.sScp s))
    bs  = Sol.cuBinds c
    k   = F.ksuKVar ksu
    su = dropUnsortedExprs g (F.ksuSubst  ksu)

-- | @cubePredExc@ computes the predicate for the subset of binders bs'.
--
-- Schematically, the result is
--
-- > Exists (bindsOf bs'). (pAnd (predicatesOf bs'))[Sol.cuSubst c]
--
-- but we also preserve the information about which variables are being
-- substituted:
--
-- > Exists (bindsOf bs'). pAnd (predicatesOf bs') && x1=e1 && ... && xn=en
--
-- where @Sol.cuSubst c = [x1:=e1;...;xn:=en]@.
--
cubePredExc :: CombinedEnv ann -> Sol.Sol Sol.QBind -> Sol.Cube -> F.IBindEnv
            -> (F.Pred, KInfo)
cubePredExc g s c bs' =
    let psu' = F.pAnd [ F.EEq (F.expr x) e | (x, e) <- M.toList m ]
        (p', kI) = apply g' s bs'
        cubeE = F.pExist yts' (F.pAndNoDedup [p', psu'])
     in (cubeE, extendKInfo kI (Sol.cuTag c))
  where
    yts' = symSorts g bs'
    g' = addCEnv  g bs
    F.Su m = dropUnsortedExprs g' (Sol.cuSubst c)
    bs = Sol.cuBinds c

isClass :: F.Sort -> Bool
isClass F.FNum  = True
isClass F.FFrac = True
isClass _       = False

combinedSEnv :: CombinedEnv a -> F.SEnv F.Sort
combinedSEnv g = F.sr_sort <$> F.fromListSEnv (F.envCs be bs)
  where
    be         = ceBEnv g
    bs         = ceIEnv g

addCEnv :: CombinedEnv a -> F.IBindEnv -> CombinedEnv a
addCEnv g bs' = g { ceIEnv = F.unionIBindEnv (ceIEnv g) bs' }

symSorts :: CombinedEnv a -> F.IBindEnv -> [(F.Symbol, F.Sort)]
symSorts g bs = second F.sr_sort <$> F.envCs (ceBEnv g) bs

_noKvars :: F.Expr -> Bool
_noKvars = null . V.kvarsExpr

--------------------------------------------------------------------------------
-- | Information about size of formula corresponding to an "eliminated" KVar.
--------------------------------------------------------------------------------
data KInfo = KI { kiTags  :: [Tag]
                , kiDepth :: !Int
                , kiCubes :: !Integer
                } deriving (Eq, Ord, Show)

instance Semigroup KInfo where
  ki <> ki' = KI ts d s
    where
      ts    = appendTags (kiTags  ki) (kiTags  ki')
      d     = max        (kiDepth ki) (kiDepth ki')
      s     = (*)        (kiCubes ki) (kiCubes ki')

instance Monoid KInfo where
  mempty  = KI [] 0 1
  mappend = (<>)

mplusKInfo :: KInfo -> KInfo -> KInfo
mplusKInfo ki ki' = (mappend ki ki') { kiCubes = kiCubes ki + kiCubes ki'}

mconcatPlus :: [KInfo] -> KInfo
mconcatPlus = foldr mplusKInfo mempty

appendTags :: [Tag] -> [Tag] -> [Tag]
appendTags ts ts' = Misc.sortNub (ts ++ ts')

extendKInfo :: KInfo -> F.Tag -> KInfo
extendKInfo ki t = ki { kiTags  = appendTags [t] (kiTags  ki)
                      , kiDepth = 1  +            kiDepth ki }
