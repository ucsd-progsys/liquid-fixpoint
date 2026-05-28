{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wno-orphans   #-}

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
  , subst1
  , substa
  , substf
  , substSymbolsSet
  , Refreshable(..)
  , Subable(..)
  , subst
  , rapierSubstExpr
  , filterSubst
  , catSubst
  , exprSymbolsSet
  , extendSubst
  , extendSubstWithVar
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

-- | The Subable class provides overloaded names to compute the free symbols of
-- a value, and to perform capture-avoiding substitution on it.
class (Eq (Variable a), Hashable (Variable a)) => Subable a where
  type Variable a
  type Variable a = Symbol

  -- | Free symbols of a value
  syms   :: a -> S.HashSet (Variable a)

  -- | Capture-avoiding substitution of a value, given a scope set of symbols
  -- that are allowed to appear free in the result.
  --
  -- When a binder's name is in the scope set, it is renamed to a fresh name.
  -- In this way, it cannot capture free variables in the range of the
  -- substitution as these free variables need to be necessarily in scope.
  --
  -- The fresh name is chosen so it does not appear in the scope set, and
  -- therefore the scope set must really contain all names in scope so the new
  -- "fresh" name does not accidentally capture free variables in the term
  -- on which the substitution is applied.
  --
  substr :: HasCallStack => S.HashSet (Variable a) -> SubstV (Variable a) -> a -> a

subst :: (HasCallStack, Subable a) => SubstV (Variable a) -> a -> a
subst su e = substr ns su e
  where
    ns = substSymbolsSet su `S.union` syms e

instance Subable a => Subable (Located a) where
  type Variable (Located a) = Variable a
  syms (Loc _ _ x)   = syms x
  substr ns m (Loc l l' x) = Loc l l' (substr ns m x)

instance Subable () where
  syms _         = S.empty
  substr _ _ ()  = ()

instance (Subable a, Subable b, Variable a ~ Variable b) => Subable (a,b) where
  type Variable (a, b) = Variable a

  syms  (x, y)      = S.union (syms x) (syms y)
  substr ns su (x,y) = (substr ns su x, substr ns su y)

instance Subable a => Subable [a] where
  type Variable [a] = Variable a
  syms             = S.unions . map syms
  substr ns su     = fmap (substr ns su)

instance Subable a => Subable (Maybe a) where
  type Variable (Maybe a) = Variable a
  syms             = maybe S.empty syms
  substr ns m      = fmap (substr ns m)


instance Subable a => Subable (M.HashMap k a) where
  type Variable (M.HashMap k a) = Variable a
  syms             = syms . M.elems
  substr ns su     = M.map (substr ns su)

subst1 :: Subable a => a -> (Variable a, ExprBV (Variable a) (Variable a)) -> a
subst1 y (x, e) = subst (Su $ M.fromList [(x, e)]) y

substa :: Subable a => (Variable a -> Variable a) -> a -> a
substa f = substf (EVar . f)

substf :: Subable a => (Variable a -> ExprBV (Variable a) (Variable a)) -> a -> a
substf f e = subst (Su $ M.mapWithKey (const . f) $ S.toMap $ syms e) e

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

instance (Eq v, Hashable v, Refreshable v) => Subable (ExprBV v v) where
  type Variable (ExprBV v v) = v
  syms  = exprSymbolsSet
  substr = rapierSubstExpr

--- | Variable names for which we can propose variations to avoid name captures
class Refreshable v where
  -- | Variations of a variable name. They must contain at least a fresh name in
  -- the contexts where @candidates@ is used.
  candidates :: v -> [v]

instance Refreshable Symbol where
  candidates x =
     let (x', i) = splitIntSuffix x
      in x : map (intSymbol x') [i..]
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
  :: (HasCallStack, Eq v, Hashable v, Refreshable v)
  => S.HashSet v -> SubstV v -> ExprBV v v -> ExprBV v v
rapierSubstExpr = go
  where
    go s su e0 = case e0 of
        EApp f e -> EApp (go s su f) (go s su e)
        ELam (x, t) e ->
          let (s', x') = freshInNS x s
              su' = extendSubstWithVar su x x'
           in ELam (x', t) (go s' su' e)
        ELet x e1 e2 ->
          let (s', x') = freshInNS x s
              su' = extendSubstWithVar su x x'
           in ELet x' (go s su e1) (go s' su' e2)

        ECoerc a t e -> ECoerc a t (go s su e)
        ENeg e -> ENeg (go s su e)
        EBin op e1 e2 -> EBin op (go s su e1) (go s su e2)
        EIte p e1 e2 -> EIte (go s su p) (go s su e1) (go s su e2)
        ECst e so -> ECst (go s su e) so
        EVar x
          | S.member x s -> appSubst su x
          | otherwise -> error "rapierSubstExpr: variable not in scope set"
        PAnd ps -> PAnd $ map (go s su) ps
        POr ps -> POr $ map (go s su) ps
        PNot p -> PNot $ go s su p
        PImp p1 p2 -> PImp (go s su p1) (go s su p2)
        PIff p1 p2 -> PIff (go s su p1) (go s su p2)
        PAtom r e1 e2 -> PAtom r (go s su e1) (go s su e2)
        PKVar k tsu su' -> PKVar k tsu (catSubstGo su' su s)
        PAll bs p ->
          let xs = map fst bs
              (s', fs) = freshInNSL xs s
              su' = List.foldl' (\su1 (x, x') -> extendSubstWithVar su1 x x') su (zip xs fs)
              bs' = zip fs (map snd bs)
           in
              PAll bs' $ go s' su' p
        PExist bs p ->
          let xs = map fst bs
              (s', fs) = freshInNSL xs s
              su' = List.foldl' (\su1 (x, x') -> extendSubstWithVar su1 x x') su (zip xs fs)
              bs' = zip fs (map snd bs)
           in
              PExist bs' $ go s' su' p
        p -> p

    catSubstGo su1 su2 s = catKVarSubst su1' (toListSubst su2)
      where
        su1' = mapKVarSubst (rapierSubstExpr s su2) su1

extendSubst :: Hashable v => SubstV v -> v -> ExprBV v v -> SubstV v
extendSubst (Su m) x e = Su $ M.insert x e m

-- | Like 'extendSubst', but a no-op when @x' == x@ and @x@ is not already in
-- the substitution domain. Use when extending with a binder rename @x → EVar x'@
-- that may be trivial.
extendSubstWithVar :: (Eq v, Hashable v) => SubstV v -> v -> v -> SubstV v
extendSubstWithVar su@(Su m) x x'
  | x' == x && not (M.member x m) = su
  | otherwise = extendSubst su x (EVar x')

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
  substr ns su (Reft (v, ras)) =
     let (ns', v') = freshInNS v ns
         su' = extendSubstWithVar su v v'
      in
         Reft (v', substr ns' su' ras)

reftSymbolsSet :: (Eq v, Hashable v) => ReftBV v v -> S.HashSet v
reftSymbolsSet (Reft (v, ras)) = S.delete v $ exprSymbolsSet ras

instance Subable SortedReft where
  syms               = syms . sr_reft
  substr ns su (RR so r) = RR so $ substr ns su r

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
