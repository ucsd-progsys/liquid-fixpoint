{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}

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

import           Control.Parallel.Strategies
import           Control.Arrow (second, (***))
import           Control.Monad (void)
import           Control.Monad.Reader
import qualified Data.HashSet                   as S
import qualified Data.HashMap.Strict            as M
import qualified Data.List                      as L
import           Data.Maybe                     (fromMaybe, maybeToList, isNothing)
import qualified Data.Bifunctor                 as Bifunctor (second)
import           Language.Fixpoint.Types.PrettyPrint ()
import           Language.Fixpoint.Types.Visitor      as V
import           Language.Fixpoint.SortCheck          (ElabM)
import qualified Language.Fixpoint.SortCheck          as So
import qualified Language.Fixpoint.Misc               as Misc
import           Language.Fixpoint.Types.Config
import qualified Language.Fixpoint.Types              as F
import           Language.Fixpoint.Types                 ((&.&))
import qualified Language.Fixpoint.Types.Solutions    as Sol
import           Language.Fixpoint.Types.Constraints  hiding (ws, bs)
import           Prelude                              hiding (init, lookup)
import           Language.Fixpoint.Solver.Sanitize

-- DEBUG
import Text.Printf (printf)
-- import Debug.Trace (trace)


--------------------------------------------------------------------------------
-- | Initial Solution (from Qualifiers and WF constraints) ---------------------
--------------------------------------------------------------------------------
init :: (F.Fixpoint a) => Config -> F.SInfo a -> S.HashSet F.KVar -> Sol.Solution
--------------------------------------------------------------------------------
init cfg si ks_ = Sol.fromList symEnv keqs [] mempty ebs xEnv
  where
    keqs       = runReader (traverse (refine si qcs genv) ws) (solverFlags $ solver cfg) `using` parList rdeepseq
    qcs        = {- trace ("init-qs-size " ++ show (length ws, length qs_, M.keys qcs_)) $ -} qcs_
    qcs_       = mkQCluster qs_
    qs_        = F.quals si
    ws         = [ w | (k, w) <- M.toList (F.ws si), k `S.member` ks ]
    ks         = {- trace ("init-ks-size" ++ show (S.size ks_)) $ -} ks_
    genv       = initQualifierEnv cfg si
    symEnv     = symbolEnv cfg si
    ebs        = ebindInfo si
    xEnv       = F.fromListSEnv [ (x, (i, F.sr_sort sr)) | (i,(x,sr,_)) <- F.bindEnvToList (F.bs si)]

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

refine :: F.SInfo a -> QCluster -> F.SEnv F.Sort -> F.WfC a -> ElabM (F.KVar, Sol.QBind)
refine info qs genv w = refineK (allowHOquals info) env qs (F.wrft w)
  where
    env             = wenvSort <> genv
    wenvSort        = F.sr_sort <$> F.fromListSEnv (F.envCs (F.bs info) (F.wenv w))

instConstants :: F.SInfo a -> F.SEnv F.Sort
instConstants = F.fromListSEnv . filter notLit . F.toListSEnv . F.gLits
  where
    notLit    = not . F.isLitSymbol . fst


refineK :: Bool -> F.SEnv F.Sort -> QCluster -> (F.Symbol, F.Sort, F.KVar) -> ElabM (F.KVar, Sol.QBind)
refineK ho env qs (v, t, k) =
  do eqs' <- Sol.qbFilterM (okInst env v t) eqs
     pure $ F.notracepp _msg (k, eqs')
   where
    eqs                     = instK ho env v t qs

    _msg                    = printf "\n\nrefineK: k = %s, eqs = %s" (F.showpp k) (F.showpp eqs)

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
  => (F.Expr -> F.Expr)
  -> F.IBindEnv
  -> F.BindEnv a
  -> Sol.Solution
  -> F.SimpC a
  -> F.Expr
lhsPred elab bindingsInSmt be s c =
    let ap = apply elab g s bs
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

apply
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.IBindEnv -> ExprInfo
apply elab g s bs =
    -- Clear the "known" bindings for applyKVars, since it depends on
    -- using the fully expanded representation of the predicates to bind their
    -- variables with quantifiers.
    let xrs = map (lookupBindEnvExt elab g s) (F.elemsIBindEnv bs)
        (ps,  ks) = envConcKVars xrs
        (pks, kI) = applyKVars elab g {ceBindingsInSmt = F.emptyIBindEnv} s ks
     in (F.conj (pks:ps), kI)   -- see [NOTE: pAnd-SLOW]

-- | @applyInSortedReft@ applies the solution to a single sorted reft
applyInSortedReft
  :: CombinedEnv ann
  -> Sol.Sol Sol.QBind
  -> (F.Symbol, F.SortedReft)
  -> (F.Symbol, F.SortedReft)
applyInSortedReft g s xsr@(x, sr) =
    let (ps,  ks) = envConcKVars [xsr]
        (pks, _) = applyKVars id g {ceBindingsInSmt = F.emptyIBindEnv} s ks
     in (x, sr { F.sr_reft = F.Reft (x, F.conj (pks : ps)) })

-- | Produces conjuncts of each sorted reft in the IBindEnv, separated
-- into concrete conjuncts and kvars.
envConcKVars :: [(F.Symbol, F.SortedReft)] -> ([F.Expr], [F.KVSub])
envConcKVars xrs =
  let (pss, kss) = unzip [ F.sortedReftConcKVars x sr | (x, sr) <- xrs ]
   in (concat pss, concat kss)

lookupBindEnvExt
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.BindId -> (F.Symbol, F.SortedReft)
lookupBindEnvExt elab g s i =
    let msol = ebSol elab (g {ceBindingsInSmt = F.emptyIBindEnv}) s i
     in (x, case msol of
              Just p -> sr { F.sr_reft = F.Reft (x, p) }
              Nothing -> if F.memberIBindEnv i (ceBindingsInSmt g)
                           then sr { F.sr_reft = F.Reft (x, F.EVar (F.bindSymbol (fromIntegral i)))}
                           else sr)
   where
      (x, sr, _)              = F.lookupBindEnv i (ceBEnv g)

ebSol
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.BindId -> Maybe F.Expr
ebSol elab g sol bindId = case M.lookup bindId sebds of
  Just (Sol.EbSol p)    -> Just p
  Just (Sol.EbDef cs _) ->
      let cSol c = if sid c == ceCid g
                     then F.PFalse
                     else let p = ebindReft elab g s' c
                           in exElim (Sol.sxEnv s') (senv c) bindId p
          exps = map cSol cs
       in Just $ F.PAnd exps
  _                     -> Nothing
  where
    sebds = Sol.sEbd sol
    s' = sol { Sol.sEbd = M.insert bindId Sol.EbIncr sebds }

ebindReft
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.SimpC () -> F.Pred
ebindReft elab g s c =
    let a = apply elab g' s bs
     in F.pAnd [ fst a , F.crhs c ]
  where
    g'          = g { ceCid = sid c, ceIEnv = bs }
    bs          = F.senv c

exElim :: F.SEnv (F.BindId, F.Sort) -> F.IBindEnv -> F.BindId -> F.Pred -> F.Pred
exElim env ienv xi p = F.notracepp msg (F.pExist yts p)
  where
    msg         = "exElim" -- printf "exElim: ix = %d, p = %s" xi (F.showpp p)
    yts         = [ (y, yt) | y        <- F.syms p
                            , (yi, yt) <- maybeToList (F.lookupSEnv y env)
                            , xi < yi
                            , yi `F.memberIBindEnv` ienv                  ]

applyKVars
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> [F.KVSub] -> ExprInfo
applyKVars elab g s ks =
  let bcs = map (applyKVar elab g s) ks
      (es, is) = unzip bcs
   in (F.pAndNoDedup es, mconcat is)

applyKVar
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> ExprInfo
applyKVar elab g s ksu = case Sol.lookup s (F.ksuKVar ksu) of
  Left cs   -> hypPred elab g s ksu cs
  Right eqs -> let qbp = Sol.qbPreds (F.ksuSubst ksu) eqs
                in (F.pAndNoDedup $ fst <$> qbp, mempty) -- TODO: don't initialize kvars that have a hyp solution

mkNonCutsExpr :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVar -> Sol.Hyp -> F.Expr
mkNonCutsExpr ce s k cs =
  let bcps = map (bareCubePred ce s k) cs
   in F.pOr bcps

nonCutsResult :: F.BindEnv ann -> Sol.Sol Sol.QBind -> M.HashMap F.KVar F.Expr
nonCutsResult be s = M.mapWithKey (mkNonCutsExpr g s) $ Sol.sHyp s
  where
    g = CEnv Nothing be F.emptyIBindEnv F.dummySpan F.emptyIBindEnv


-- | Produces a predicate from a constraint defining a kvar.
--
-- This is written in imitation of 'cubePred'. However, there are some
-- differences since the result of 'cubePred' is fed to the verification
-- pipeline and @bareCubePred@ is meant for human inspection.
--
-- 1) Only one existential quantifier is introduced at the top of the
--    expression.
-- 2) @bareCubePred@ doesn't elaborate the expression, so it avoids calling
--    'elabExist'. 'apply' is invoked to eliminate other kvars though, and
--    apply will invoke 'elabExist', so 'Liquid.Fixpoint.SortCheck.unElab'
--    might need to be called on the output to remove the elaboration.
-- 3) The expression is created from its defining constraints only, while
--    @cubePred@ does expect the caller to supply the substitution at a
--    particular use of the KVar. Thus @cubePred@ produces a different
--    expression for every use site of the kvar, while here we produce one
--    expression for all the uses.
-- NOTE: In this case, we *keep* the `xts` "free" in the final predicate. as
-- That is, we only quantify out the `yts` as, in this use-case, the `xts`
-- are the "parameters" of the KVar that we want to leave in to make
-- explicit what equalities those parameters have in each cube.

bareCubePred
  :: CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVar -> Sol.Cube -> F.Expr
bareCubePred g s k c =
    let (_xts, psu) = substElim id (Sol.sEnv s) sEnv g' su
        (p, _kI) = apply id g' s bs'
     in F.pExist yts (psu &.& p)
  where
    bs = Sol.cuBinds c
    su = Sol.cuSubst c
    g' = addCEnv  g bs
    bs' = F.diffIBindEnv bs (Misc.safeLookup "sScp" k (Sol.sScp s))
    yts = symSorts g bs'
    sEnv = F.seSort (Sol.sEnv s)

hypPred
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> Sol.Hyp -> ExprInfo
hypPred elab g s ksu hyp =
  let cs = map (cubePred elab g s ksu) hyp
   in F.pOr *** mconcatPlus $ unzip cs

{- | `cubePred g s k su c` returns the predicate for

        (k . su)

      defined by using cube

        c := [b1,...,bn] |- (k . su')

      in the binder environment `g`. The binders in `sScp s k` are not included
      in the final predicate. They are considered redundant conjuncts as per
      section 2.4 of "Local Refinement Typing", ICFP 2017.
 -}
cubePred
  :: (F.Expr -> F.Expr) -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> Sol.Cube -> ExprInfo
cubePred elab g s ksu c    =
    let ((xts,psu,p), kI) = cubePredExc elab g s ksu c bs'
     in (F.pExist xts (psu &.& p), kI)
  where
    bs' = F.diffIBindEnv bs (Misc.safeLookup "sScp" k (Sol.sScp s))
    bs  = Sol.cuBinds c
    k   = F.ksuKVar ksu

type Binders = [(F.Symbol, F.Sort)]

-- | @cubePredExc@ computes the predicate for the subset of binders bs'.
--   The output is a tuple, `(xts, psu, p, kI)` such that the actual predicate
--   we want is `Exists xts. (psu /\ p)`.

cubePredExc :: (F.Expr -> F.Expr)
            -> CombinedEnv ann -> Sol.Sol Sol.QBind -> F.KVSub -> Sol.Cube -> F.IBindEnv
            -> ((Binders, F.Pred, F.Pred), KInfo)
cubePredExc elab g s ksu c bs' =
    let (xts, psu)  = substElim elab (Sol.sEnv s) sEnv g  su
        (_  , psu') = substElim elab (Sol.sEnv s) sEnv g' su'
        (p', kI) = apply elab g' s bs'
        cubeE = F.pExist yts' (F.pAndNoDedup [p', psu'])
        cubeP = (xts, psu, cubeE)
     in (cubeP, extendKInfo kI (Sol.cuTag c))
  where
    yts'            = symSorts g bs'
    g'              = addCEnv  g bs
    su'             = Sol.cuSubst c
    bs              = Sol.cuBinds c
    su              = F.ksuSubst  ksu
    sEnv            = F.insertSEnv (F.ksuVV ksu) (F.ksuSort ksu) (F.seSort $ Sol.sEnv s)

-- TODO: SUPER SLOW! Decorate all substitutions with Sorts in a SINGLE pass.

{- | @substElim@ returns the binders that must be existentially quantified,
     and the equality predicate relating the kvar-"parameters" and their
     actual values. i.e. given

        K[x1 := e1]...[xn := en]

     where e1 ... en have types t1 ... tn
     we want to quantify out

       x1:t1 ... xn:tn

     and generate the equality predicate && [x1 ~~ e1, ... , xn ~~ en]
     we use ~~ because the param and value may have different sorts, see:

        tests/pos/kvar-param-poly-00.hs

     Finally, we filter out binders if they are

     1. "free" in e1...en i.e. in the outer environment.
        (Hmm, that shouldn't happen...?)

     2. are binders corresponding to sorts (e.g. `a : num`, currently used
        to hack typeclasses current.)
 -}
substElim
  :: (F.Expr -> F.Expr) -> F.SymEnv -> F.SEnv F.Sort -> CombinedEnv a -> F.Subst -> ([(F.Symbol, F.Sort)], F.Pred)
substElim elab syEnv sEnv g (F.Su m) =
    let p = map (\(x, e ,t) -> mkSubst elab syEnv x (substSort sEnv x) e t) xets
     in (xts, F.pAnd p)
  where
    xts    = [ (x, t)    | (x, _, t) <- xets, not (S.member x frees) ]
    xets   = [ (x, e, t) | (x, e)    <- xes, t <- sortOf e, not (isClass t)]
    frees  = S.fromList (concatMap (F.syms . snd) xes)
    sortOf = maybeToList . So.checkSortExpr sp env
    sp     = ceSpan g
    xes    = M.toList m
    env    = combinedSEnv g

substSort :: F.SEnv F.Sort -> F.Symbol -> F.Sort
substSort sEnv sym = fromMaybe (err sym) $ F.lookupSEnv sym sEnv
  where
    err x = error $ "Solution.substSort: unknown binder " ++ F.showpp x


-- LH #1091
mkSubst
  :: (F.Expr -> F.Expr) -> F.SymEnv -> F.Symbol -> F.Sort -> F.Expr -> F.Sort -> F.Expr
mkSubst elab env x tx ey ty
  | tx == ty    = F.EEq ex ey
  | otherwise   = F.EEq (elab $ So.toInt env ex tx) (elab $ So.toInt env ey ty)
  where
    ex          = F.expr x

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

mplus :: KInfo -> KInfo -> KInfo
mplus ki ki' = (mappend ki ki') { kiCubes = kiCubes ki + kiCubes ki'}

mconcatPlus :: [KInfo] -> KInfo
mconcatPlus = foldr mplus mempty

appendTags :: [Tag] -> [Tag] -> [Tag]
appendTags ts ts' = Misc.sortNub (ts ++ ts')

extendKInfo :: KInfo -> F.Tag -> KInfo
extendKInfo ki t = ki { kiTags  = appendTags [t] (kiTags  ki)
                      , kiDepth = 1  +            kiDepth ki }

--------------------------------------------------------------------------------
-- | `ebindInfo` constructs the information about the "ebind-definitions".
--------------------------------------------------------------------------------
ebindInfo :: F.SInfo a -> [(F.BindId, Sol.EbindSol)]
ebindInfo si = group [((bid, x), cons cid) | (bid, cid, x) <- ebindDefs si]
  where cons cid = void (Misc.safeLookup "ebindInfo" cid cs)
        cs = F.cm si
        cmpByFst x y = fst ( fst x ) == fst ( fst y )
        group xs = (\ys -> Bifunctor.second (Sol.EbDef (snd <$> ys)) (fst $ head ys))
                    <$> L.groupBy cmpByFst xs

ebindDefs :: F.SInfo a -> [(F.BindId, F.SubcId, F.Symbol)]
ebindDefs si = [ (bid, cid, x) | (cid, x) <- cDefs
                               , bid      <- maybeToList (M.lookup x ebSyms)]
  where
    ebSyms   = ebindSyms si
    cDefs    = cstrDefs  si

ebindSyms :: F.SInfo a -> M.HashMap F.Symbol F.BindId
ebindSyms si = M.fromList [ (xi, bi) | bi        <- ebinds si
                                     , let (xi,_,_) = F.lookupBindEnv bi be ]
  where
    be       = F.bs si

cstrDefs :: F.SInfo a -> [(F.SubcId, F.Symbol)]
cstrDefs si = [(cid, x) | (cid, c) <- M.toList (cm si)
                        , x <- maybeToList (cstrDef be c) ]
  where
    be      = F.bs si

cstrDef :: F.BindEnv a -> F.SimpC a -> Maybe F.Symbol
cstrDef be c
  | Just (F.EVar x) <- e = Just x
  | otherwise            = Nothing
  where
    (v,_,_)              = F.lookupBindEnv (cbind c) be
    e                    = F.notracepp _msg $ F.isSingletonExpr v rhs
    _msg                 = "cstrDef: " ++ show (stag c) ++ " crhs = " ++ F.showpp rhs
    rhs                  = V.stripCasts (crhs c)
