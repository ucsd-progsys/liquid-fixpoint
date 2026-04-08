{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wno-orphans   #-}
{-# LANGUAGE InstanceSigs #-}

-- | This module contains the various instances for Subable,
--   which (should) depend on the visitors, and hence cannot
--   be in the same place as the @Term@ definitions.
module Language.Fixpoint.Types.Substitutions (
    mkSubst
  , mkKVarSubst
  , substFromKSubst
  , kSubstFromSubst
  , ksubst
  , isEmptySubst
  , substExcept
  , substfExcept
  , subst1Except
  , substSymbolsSet
  , Refreshable(..)
  , rapierSubstExpr
  , targetSubstSyms
  , filterSubst
  , catSubst
  , exprSymbolsSet
  , extendSubst
  , meetReft
  , pprReft
  ) where

import           Data.List                 as List
import           Data.Maybe
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Language.Fixpoint.Types.Binders
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Refinements
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ.Compat
import           Text.Printf               (printf)

instance (Eq v, Hashable v) => Semigroup (SubstV v) where
  (<>) = catSubst

instance (Eq v, Hashable v) => Monoid (SubstV v) where
  mempty  = emptySubst
  mappend = (<>)

instance Semigroup (KVarSubst Symbol Symbol) where
  su1 <> su2 = kSubstFromSubst $ substFromKSubst su1 <> substFromKSubst su2

instance Monoid (KVarSubst Symbol Symbol) where
  mempty = kSubstFromSubst mempty
  mappend = (<>)

substFromKSubst :: Hashable v => KVarSubst v v -> SubstV v
substFromKSubst = Su . fromKVarSubst

kSubstFromSubst :: SubstV v -> KVarSubst v v
kSubstFromSubst (Su m) = toKVarSubst m

ksubst :: KVarSubst Symbol Symbol -> Expr -> Expr
ksubst = subst . substFromKSubst

filterSubst :: (v -> ExprBV v v -> Bool) -> SubstV v -> SubstV v
filterSubst f (Su m) = Su (M.filterWithKey f m)

emptySubst :: SubstV v
emptySubst = Su M.empty

catSubst :: (Eq v, Hashable v) => SubstV v -> SubstV v -> SubstV v
catSubst (Su s1) θ2@(Su s2) = Su $ M.union s1' s2
  where
    s1'                     = subst θ2 <$> s1

mkSubst :: Hashable v => [(v, ExprBV v v)] -> SubstV v
mkSubst = Su . M.fromList . reverse . filter notTrivial
  where
    notTrivial (x, EVar y) = x /= y
    notTrivial _           = True

mkKVarSubst :: [(Symbol, Expr)] -> KVarSubst Symbol Symbol
mkKVarSubst = kSubstFromSubst . mkSubst

isEmptySubst :: SubstV v -> Bool
isEmptySubst (Su xes) = M.null xes

targetSubstSyms :: (Eq v, Hashable v) => SubstV v -> [v]
targetSubstSyms (Su ms) = syms $ M.elems ms

substSymbolsSet :: (Eq v, Hashable v) => SubstV v -> S.HashSet v
substSymbolsSet (Su m) = S.unions $ map exprSymbolsSet (M.elems m)

instance Subable () where
  syms _      = []
  subst _ ()  = ()
  substf _ () = ()
  substa _ () = ()

instance (Subable a, Subable b, Variable a ~ Variable b) => Subable (a,b) where
  type Variable (a, b) = Variable a
  syms  (x, y)   = syms x ++ syms y
  subst su (x,y) = (subst su x, subst su y)
  substf f (x,y) = (substf f x, substf f y)
  substa f (x,y) = (substa f x, substa f y)

instance Subable a => Subable [a] where
  type Variable [a] = Variable a
  syms   = concatMap syms
  subst  = fmap . subst
  substf = fmap . substf
  substa = fmap . substa

instance Subable a => Subable (Maybe a) where
  type Variable (Maybe a) = Variable a
  syms   = concatMap syms . maybeToList
  subst  = fmap . subst
  substf = fmap . substf
  substa = fmap . substa


instance Subable a => Subable (M.HashMap k a) where
  type Variable (M.HashMap k a) = Variable a
  syms   = syms . M.elems
  subst  = M.map . subst
  substf = M.map . substf
  substa = M.map . substa

subst1Except :: Subable a => [Variable a] -> a -> (Variable a, ExprBV (Variable a) (Variable a)) -> a
subst1Except xs z su@(x, _)
  | x `elem` xs = z
  | otherwise   = subst1 z su

substfExcept :: Eq v => (v -> ExprBV b v) -> [v] -> v -> ExprBV b v
substfExcept f xs y = if y `elem` xs then EVar y else f y

substExcept  :: Eq v => SubstV v -> [v] -> SubstV v
-- substExcept  (Su m) xs = Su (foldr M.delete m xs)
substExcept (Su xes) xs = Su $ M.filterWithKey (const . not . (`elem` xs)) xes

instance Subable Symbol where
  substa f                 = f
  substf f x               = subSymbol (Just (f x)) x
  subst su x               = subSymbol (Just $ appSubst su x) x -- subSymbol (M.lookup x s) x
  syms x                   = [x]

appSubst :: (Eq v, Hashable v) => SubstV v -> v -> ExprBV v v
appSubst (Su s) x = fromMaybe (EVar x) (M.lookup x s)

subSymbol :: (Ord v, Hashable v, Fixpoint v) => Maybe (ExprBV v v) -> v -> v
subSymbol (Just (EVar y)) _ = y
subSymbol Nothing         x = x
subSymbol a               b = errorstar (printf "Cannot substitute symbol %s with expression %s" (showFix b) (showFix a))

captureAvoiding :: Eq v => v -> (v -> ExprBV b v) -> v -> ExprBV b v
captureAvoiding x f y = if y == x then EVar x else f y

instance (Eq v, Hashable v) => Subable (ExprBV v v) where
  type Variable (ExprBV v v) = v
  syms                     = exprSymbols
  substa f                 = substf (EVar . f)
  substf :: (v -> ExprBV v v) -> ExprBV v v -> ExprBV v v
  substf f (EApp s e)      = EApp (substf f s) (substf f e)
  substf f (ELam (x,t) e)  = ELam (x, t) (substf (captureAvoiding x f) e)
  substf f (ECoerc a t e)  = ECoerc a t (substf f e)
  substf f (ENeg e)        = ENeg (substf f e)
  substf f (EBin op e1 e2) = EBin op (substf f e1) (substf f e2)
  substf f (ELet x e1 e2)  = ELet x (substf f e1) (substf (captureAvoiding x f) e2)
  substf f (EIte p e1 e2)  = EIte (substf f p) (substf f e1) (substf f e2)
  substf f (ECst e so)     = ECst (substf f e) so
  substf f (EVar x)        = f x
  substf f (PAnd ps)       = PAnd $ map (substf f) ps
  substf f (POr  ps)       = POr  $ map (substf f) ps
  substf f (PNot p)        = PNot $ substf f p
  substf f (PImp p1 p2)    = PImp (substf f p1) (substf f p2)
  substf f (PIff p1 p2)    = PIff (substf f p1) (substf f p2)
  substf f (PAtom r e1 e2) = PAtom r (substf f e1) (substf f e2)
  substf f (PKVar k su)    = PKVar k (mapKVarSubst (substf f) su)
  substf _ (PAll _ _)      = errorstar "substf: FORALL"
  substf f (PExist xts e)  = PExist xts (substf f e)
  substf _  p              = p


  subst = go
    where
      -- The auxiliary go function skips the HasCallStack constraint on every
      -- recursive call. In case of error, the call stack only contains the
      -- point at which subst was first called.
      go su e0 = case e0 of
        EApp f e ->
          EApp (go su f) (go su e)
        ELam x e ->
          let su' = removeSubst su (fst x)
           in ELam x (go su' e)
        ELet x e1 e2 ->
          let su' = removeSubst su x
           in ELet x (go su e1) (go su' e2)
        ECoerc a t e ->
          ECoerc a t (go su e)
        ENeg e ->
          ENeg (go su e)
        EBin op e1 e2 ->
          EBin op (go su e1) (go su e2)
        EIte p e1 e2 ->
          EIte (go su p) (go su e1) (go su e2)
        ECst e so ->
          ECst (go su e) so
        EVar x ->
          appSubst su x
        PAnd ps ->
          PAnd $ map (go su) ps
        POr  ps ->
          POr  $ map (go su) ps
        PNot p ->
          PNot $ go su p
        PImp p1 p2 ->
          PImp (go su p1) (go su p2)
        PIff p1 p2 ->
          PIff (go su p1) (go su p2)
        PAtom r e1 e2 ->
          PAtom r (go su e1) (go su e2)
        PKVar k su' ->
          PKVar k (kSubstFromSubst $ substFromKSubst su' `catSubst` su)
        PAll bs p
          | disjointRange su' bs ->
            PAll bs $ go su' p
          | otherwise ->
            errorstar "subst: PAll (without disjoint binds)"
          where
            su' = substExcept su (map fst bs)

        PExist bs p
          | disjointRange su' bs ->
            PExist bs $ go su' p
          | otherwise ->
            errorstar "subst: EXISTS without disjoint binds"
          where
            su' = substExcept su (map fst bs)
        p ->
          p

removeSubst :: (Eq v, Hashable v) => SubstV v -> v -> SubstV v
removeSubst (Su su) x = Su $ M.delete x su

-- | Variable names for which we can propose variations to avoid name captures
class Refreshable v where
  -- | Variations of a variable name. They must contain at least a fresh name in
  -- the contexts where @candidates@ is used.
  candidates :: v -> [v]

instance Refreshable Symbol where
  candidates x = [ renameSubstSymbol x i | i <- [0..] ]

-- | Rapier style capture-avoiding substitution
--
-- The scope set parameter must contain any symbols that are expected
-- to appear free in the result expression. Typically, this is the set of
-- symbols that are free in the range of the substitution, plus any symbols
-- that are already free in the input expression.
rapierSubstExpr :: (Hashable v, Refreshable v) => S.HashSet v -> SubstV v -> ExprBV v v -> ExprBV v v
rapierSubstExpr s su e0 =
  let go = rapierSubstExpr
   in case e0 of
    EApp f e -> EApp (go s su f) (go s su e)
    ELam (x, t) e ->
      if x `S.member` s then
        let x' = fresh x
            su' = extendSubst su x (EVar x')
         in ELam (x', t) (go (S.insert x' s) su' e)
      else
        ELam (x, t) (go (S.insert x s) (removeSubst su x) e)
    ELet x e1 e2 ->
      if x `S.member` s then
        let x' = fresh x
            su' = extendSubst su x (EVar x')
         in ELet x' (go s su e1) (go (S.insert x' s) su' e2)
      else
        let su' = removeSubst su x
         in ELet x (go s su e1) (go (S.insert x s) su' e2)

    ECoerc a t e -> ECoerc a t (go s su e)
    ENeg e -> ENeg (go s su e)
    EBin op e1 e2 -> EBin op (go s su e1) (go s su e2)
    EIte p e1 e2 -> EIte (go s su p) (go s su e1) (go s su e2)
    ECst e so -> ECst (go s su e) so
    EVar x -> appSubst su x
    PAnd ps -> PAnd $ map (go s su) ps
    POr ps -> POr $ map (go s su) ps
    PNot p -> PNot $ go s su p
    PImp p1 p2 -> PImp (go s su p1) (go s su p2)
    PIff p1 p2 -> PIff (go s su p1) (go s su p2)
    PAtom r e1 e2 -> PAtom r (go s su e1) (go s su e2)
    PKVar k su' -> PKVar k (catSubstGo su' su)
    PAll bs p ->
      let mfs = map (maybeFresh . fst) bs
          fs = map (either (\x -> (x, x)) id) mfs
          su' = List.foldl' (\su1 (x, x') -> extendSubst su1 x (EVar x')) su fs
          bs' = zip (map (either id snd) mfs) (map snd bs)
          s' = foldr (S.insert . fst) s bs'
       in
          PAll bs' $ go s' su' p
    PExist bs p ->
      let mfs = map (maybeFresh . fst) bs
          fs = map (either (\x -> (x, x)) id) mfs
          su' = List.foldl' (\su1 (x, x') -> extendSubst su1 x (EVar x')) su fs
          bs' = zip (map (either id snd) mfs) (map snd bs)
          s' = foldr (S.insert . fst) s bs'
       in
          PExist bs' $ go s' su' p
    p -> p
  where
    fresh x = head $ dropWhile (`S.member` s) (candidates x)

    maybeFresh x =
      if x `S.member` s then Right (x, fresh x) else Left x

    catSubstGo su1 su2@(Su s2) = toKVarSubst $ M.union s1 s2
      where
        s1 = rapierSubstExpr s su2 <$> fromKVarSubst su1

extendSubst :: Hashable v => SubstV v -> v -> ExprBV v v -> SubstV v
extendSubst (Su m) x e = Su $ M.insert x e m

disjointRange :: (Eq v, Hashable v) => SubstV v -> [(v, Sort)] -> Bool
disjointRange (Su su) bs = S.null $ suSyms `S.intersection` bsSyms
  where
    suSyms = S.fromList $ syms (M.elems su)
    bsSyms = S.fromList $ fst <$> bs

meetReft :: Binder v => ReftBV v v -> ReftBV v v -> ReftBV v v
meetReft (Reft (v, ra)) (Reft (v', ra'))
  | v == v'          = Reft (v , pAnd [ra, ra'])
  | v == wildcard    = Reft (v', pAnd [ra', ra `subst1`  (v , EVar v')])
  | otherwise        = Reft (v , pAnd [ra, ra' `subst1` (v', EVar v )])

instance (Eq v, Hashable v, Refreshable v) => Subable (ReftBV v v) where
  type Variable (ReftBV v v) = v
  syms (Reft (v, ras))      = v : syms ras
  substa f (Reft (v, ras))  = Reft (f v, substa f ras)
  subst su (Reft (v, ras))  =
    let su' = substExcept su [v]
        s = S.union (substSymbolsSet su') (exprSymbolsSet ras)
     in Reft (v, rapierSubstExpr s su' ras)
  substf f (Reft (v, ras))  = Reft (v, substf (substfExcept f [v]) ras)
  subst1 (Reft (v, ras)) su = Reft (v, subst1Except [v] ras su)

instance Subable SortedReft where
  syms               = syms . sr_reft
  subst su (RR so r) = RR so $ subst su r
  substf f (RR so r) = RR so $ substf f r
  substa f (RR so r) = RR so $ substa f r

pprReft :: Reft -> Doc -> Doc
pprReft (Reft (v, p)) d
  | isTautoPred p
  = d
  | otherwise
  = braces (toFix v <+> colon <+> d <+> text "|" <+> ppRas [p])

-- RJ: this depends on `isTauto` hence, here.
instance (PPrint v, Fixpoint v, Ord v) => PPrint (ReftV v) where
  pprintTidy k r
    | isTautoReft r        = text "true"
    | otherwise        = pprintReft k r

instance PPrint SortedReft where
  pprintTidy k (RR so (Reft (v, ras)))
    = braces
    $ pprintTidy k v <+> text ":" <+> toFix so <+> text "|" <+> pprintTidy k ras

instance Fixpoint Reft where
  toFix = pprReftPred

instance Fixpoint SortedReft where
  toFix (RR so (Reft (v, ra)))
    = braces
    $ toFix v <+> text ":" <+> toFix so <+> text "|" <+> toFix (conjuncts ra)
  simplify (RR so (Reft (v, ra))) = RR (simplify so) (Reft (simplify v, simplify ra))

instance Show Reft where
  show = showFix

instance Show SortedReft where
  show  = showFix

pprReftPred :: Reft -> Doc
pprReftPred (Reft (_, p))
  | isTautoPred p
  = text "true"
  | otherwise
  = ppRas [p]

ppRas :: [Expr] -> Doc
ppRas = cat . punctuate comma . map toFix . flattenRefas

--------------------------------------------------------------------------------
-- | TODO: Rewrite using visitor -----------------------------------------------
--------------------------------------------------------------------------------
-- exprSymbols :: Expr -> [Symbol]
-- exprSymbols = go
  -- where
    -- go (EVar x)           = [x]
    -- go (EApp f e)         = go f ++ go e
    -- go (ELam (x,_) e)     = filter (/= x) (go e)
    -- go (ECoerc _ _ e)     = go e
    -- go (ENeg e)           = go e
    -- go (EBin _ e1 e2)     = go e1 ++ go e2
    -- go (EIte p e1 e2)     = exprSymbols p ++ go e1 ++ go e2
    -- go (ECst e _)         = go e
    -- go (PAnd ps)          = concatMap go ps
    -- go (POr ps)           = concatMap go ps
    -- go (PNot p)           = go p
    -- go (PIff p1 p2)       = go p1 ++ go p2
    -- go (PImp p1 p2)       = go p1 ++ go p2
    -- go (PAtom _ e1 e2)    = exprSymbols e1 ++ exprSymbols e2
    -- go (PKVar _ (Su su))  = syms (M.elems su)
    -- go (PAll xts p)       = (fst <$> xts) ++ go p
    -- go _                  = []


exprSymbols :: (Eq v, Hashable v) => ExprBV v v -> [v]
exprSymbols = S.toList . exprSymbolsSet

instance Expression (Symbol, SortedReft) where
  expr (x, RR _ (Reft (v, r))) = subst1 (expr r) (v, EVar x)
