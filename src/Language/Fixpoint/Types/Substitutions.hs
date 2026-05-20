{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , Subable(..)
  , rapierSubstExpr
  , filterSubst
  , catSubst
  , exprSymbolsSet
  , extendSubst
  , freshInNS
  , freshInNSL
  , meetReft
  , pprReft
  ) where

import           Data.List                 as List
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import qualified Data.Text                 as T
import           GHC.Stack                 (HasCallStack)
import           Language.Fixpoint.Types.Binders
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Spans
import           Language.Fixpoint.Types.Refinements
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ.Compat

instance (Eq v, Hashable v, Refreshable v) => Semigroup (SubstV v) where
  (<>) = catSubst

instance (Eq v, Hashable v, Refreshable v) => Monoid (SubstV v) where
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

catSubst :: (Eq v, Hashable v, Refreshable v) => SubstV v -> SubstV v -> SubstV v
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

substSymbolsSet :: (Eq v, Hashable v) => SubstV v -> S.HashSet v
substSymbolsSet (Su m) = S.unions $ map exprSymbolsSet (M.elems m)

toListSubst :: SubstV v -> [(v, ExprBV v v)]
toListSubst (Su m) = M.toList m

class (Eq (Variable a), Hashable (Variable a)) => Subable a where
  type Variable a
  type Variable a = Symbol

  syms   :: a -> S.HashSet (Variable a)           -- ^ free symbols of a
  substr :: S.HashSet (Variable a) -> SubstV (Variable a) -> a -> a
  substa :: (Variable a -> Variable a) -> a -> a
  -- substa f  = substf (EVar . f)

  substf :: (Variable a -> ExprBV (Variable a) (Variable a)) -> a -> a
  subst  :: HasCallStack => SubstV (Variable a) -> a -> a
  subst su e = substr ns su e
    where
      ns = substSymbolsSet su `S.union` syms e
  subst1 :: a -> (Variable a, ExprBV (Variable a) (Variable a)) -> a
  subst1 y (x, e) = subst (Su $ M.fromList [(x,e)]) y

instance Subable a => Subable (Located a) where
  type Variable (Located a) = Variable a
  syms (Loc _ _ x)   = syms x
  substr ns m (Loc l l' x) = Loc l l' (substr ns m x)
  substa f (Loc l l' x) = Loc l l' (substa f x)
  substf f (Loc l l' x) = Loc l l' (substf f x)
  subst su (Loc l l' x) = Loc l l' (subst su x)

instance Subable () where
  syms _      = S.empty
  subst _ ()  = ()
  substr _ _ ()  = ()
  substf _ () = ()
  substa _ () = ()

instance (Subable a, Subable b, Variable a ~ Variable b) => Subable (a,b) where
  type Variable (a, b) = Variable a

  syms  (x, y)   = S.union (syms x) (syms y)
  substr ns su (x,y) = (substr ns su x, substr ns su y)
  subst su (x,y) = (subst su x, subst su y)
  substf f (x,y) = (substf f x, substf f y)
  substa f (x,y) = (substa f x, substa f y)

instance Subable a => Subable [a] where
  type Variable [a] = Variable a
  syms   = S.unions . map syms
  subst  = fmap . subst
  substf = fmap . substf
  substa = fmap . substa

instance Subable a => Subable (Maybe a) where
  type Variable (Maybe a) = Variable a
  syms = maybe S.empty syms
  subst  = fmap . subst
  substr ns m  = fmap (substr ns m)
  substf = fmap . substf
  substa = fmap . substa


instance Subable a => Subable (M.HashMap k a) where
  type Variable (M.HashMap k a) = Variable a
  syms   = syms . M.elems
  subst  = M.map . subst
  substr ns su = M.map (substr ns su)
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

appSubst :: (Eq v, Hashable v) => SubstV v -> v -> ExprBV v v
appSubst (Su s) x = M.findWithDefault (EVar x) x s

captureAvoiding :: Eq v => v -> (v -> ExprBV b v) -> v -> ExprBV b v
captureAvoiding x f y = if y == x then EVar x else f y

instance (Eq v, Hashable v, Refreshable v) => Subable (ExprBV v v) where
  type Variable (ExprBV v v) = v
  syms                     = exprSymbolsSet
  substr = rapierSubstExpr
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
  substf f (PKVar k tsu su)    = PKVar k tsu (mapKVarSubst (substf f) su)
  substf _ (PAll _ _)      = errorstar "substf: FORALL"
  substf f (PExist xts e)  = PExist xts (substf f e)
  substf _  p              = p

--- | Variable names for which we can propose variations to avoid name captures
class Refreshable v where
  -- | Variations of a variable name. They must contain at least a fresh name in
  -- the contexts where @candidates@ is used.
  candidates :: v -> [v]

instance Refreshable Symbol where
  candidates x =
     let (x', i) = splitIntSuffix x
      in x : zipWith intSymbol (repeat x') [i..]
    where
      splitIntSuffix sx =
        case T.breakOnEnd symSepName (symbolText sx) of
          (pfx, sfx)
            | T.null pfx -> (sx, 0 :: Int)
            | otherwise  -> case reads (T.unpack sfx) of
              ((i, []) : _) -> (unSuffixSymbol sx, i + 1 :: Int)
              _ -> (sx, 0 :: Int)

-- | Rapier style capture-avoiding substitution
--
-- The scope set parameter must contain any symbols that are expected
-- to appear free in the result expression. Typically, this is the set of
-- symbols that are free in the range of the substitution, plus any symbols
-- that are already free in the input expression.
rapierSubstExpr
  :: (Eq v, Hashable v, Refreshable v)
  => S.HashSet v -> SubstV v -> ExprBV v v -> ExprBV v v
rapierSubstExpr s su e0 =
  let go = rapierSubstExpr
   in case e0 of
    EApp f e -> EApp (go s su f) (go s su e)
    ELam (x, t) e ->
      let (s', x') = freshInNS x s
          su' = extendSubst su x (EVar x')
       in ELam (x', t) (go s' su' e)
    ELet x e1 e2 ->
      let (s', x') = freshInNS x s
          su' = extendSubst su x (EVar x')
       in ELet x' (go s su e1) (go s' su' e2)

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
    PKVar k tsu su' -> PKVar k tsu (catSubstGo su' su)
    PAll bs p ->
      let xs = map fst bs
          (s', fs) = freshInNSL xs s
          su' = List.foldl' (\su1 (x, x') -> extendSubst su1 x (EVar x')) su (zip xs fs)
          bs' = zip fs (map snd bs)
       in
          PAll bs' $ go s' su' p
    PExist bs p ->
      let xs = map fst bs
          (s', fs) = freshInNSL xs s
          su' = List.foldl' (\su1 (x, x') -> extendSubst su1 x (EVar x')) su (zip xs fs)
          bs' = zip fs (map snd bs)
       in
          PExist bs' $ go s' su' p
    p -> p

  where
    catSubstGo su1 su2 = catKVarSubst su1' (toListSubst su2)
      where
        su1' = mapKVarSubst (rapierSubstExpr s su2) su1

extendSubst :: Hashable v => SubstV v -> v -> ExprBV v v -> SubstV v
extendSubst (Su m) x e = Su $ M.insert x e m

meetReft :: (Refreshable v, Binder v) => ReftBV v v -> ReftBV v v -> ReftBV v v
meetReft (Reft (v, ra)) (Reft (v', ra'))
  | v == v'          = Reft (v , pAnd [ra, ra'])
  | v == wildcard    = Reft (v', pAnd [ra', ra `subst1`  (v , EVar v')])
  | otherwise        = Reft (v , pAnd [ra, ra' `subst1` (v', EVar v )])


freshInNS :: (Refreshable v, Hashable v) => v -> S.HashSet v -> (S.HashSet v, v)
freshInNS x s =
    let x' = head $ filter (not . (`S.member` s)) (candidates x)
    in (S.insert x' s, x')

freshInNSL :: (Refreshable v, Hashable v) => [v] -> S.HashSet v -> (S.HashSet v, [v])
freshInNSL xs s = mapAccumL (flip freshInNS) s xs

instance (Eq v, Hashable v, Refreshable v) => Subable (ReftBV v v) where
  type Variable (ReftBV v v) = v
  syms = reftSymbolsSet
  substa f (Reft (v, ras))  = Reft (f v, substa f ras)
  substr ns su (Reft (v, ras)) =
     let (ns', v') = freshInNS v ns
         su' = extendSubst su v (EVar v')
      in
         Reft (v', substr ns' su' ras)
  substf f (Reft (v, ras))  = Reft (v, substf (substfExcept f [v]) ras)
  subst1 (Reft (v, ras)) su = Reft (v, subst1Except [v] ras su)

reftSymbolsSet :: (Eq v, Hashable v) => ReftBV v v -> S.HashSet v
reftSymbolsSet (Reft (v, ras)) = S.delete v $ exprSymbolsSet ras

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

instance Expression (Symbol, SortedReft) where
  expr (x, RR _ (Reft (v, r))) = subst1 (expr r) (v, EVar x)
