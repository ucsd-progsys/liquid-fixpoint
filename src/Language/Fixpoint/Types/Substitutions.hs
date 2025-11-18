{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans   #-}
{-# LANGUAGE InstanceSigs #-}

-- | This module contains the various instances for Subable,
--   which (should) depend on the visitors, and hence cannot
--   be in the same place as the @Term@ definitions.
module Language.Fixpoint.Types.Substitutions (
    mkSubst
  , isEmptySubst
  , substExcept
  , substfExcept
  , subst1Except
  , substSymbolsSet
  , rapierSubstExpr
  , targetSubstSyms
  , filterSubst
  , catSubst
  , exprSymbolsSet
  , meetReft
  , pprReft
  ) where

import           Data.Either               (rights)
import           Data.List                 as List
import           Data.Maybe
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Refinements
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ.Compat
import           Text.Printf               (printf)

instance Semigroup Subst where
  (<>) = catSubst

instance Monoid Subst where
  mempty  = emptySubst
  mappend = (<>)

filterSubst :: (Symbol -> Expr -> Bool) -> Subst -> Subst
filterSubst f (Su m) = Su (M.filterWithKey f m)

emptySubst :: Subst
emptySubst = Su M.empty

catSubst :: Subst -> Subst -> Subst
catSubst (Su s1) θ2@(Su s2) = Su $ M.union s1' s2
  where
    s1'                     = subst θ2 <$> s1

mkSubst :: [(Symbol, Expr)] -> Subst
mkSubst = Su . M.fromList . reverse . filter notTrivial
  where
    notTrivial (x, EVar y) = x /= y
    notTrivial _           = True

isEmptySubst :: Subst -> Bool
isEmptySubst (Su xes) = M.null xes

targetSubstSyms :: Subst -> [Symbol]
targetSubstSyms (Su ms) = syms $ M.elems ms

substSymbolsSet :: Subst -> S.HashSet Symbol
substSymbolsSet (Su m) = S.unions $ map exprSymbolsSet (M.elems m)

instance Subable () where
  syms _      = []
  subst _ ()  = ()
  substf _ () = ()
  substa _ () = ()

instance (Subable a, Subable b) => Subable (a,b) where
  syms  (x, y)   = syms x ++ syms y
  subst su (x,y) = (subst su x, subst su y)
  substf f (x,y) = (substf f x, substf f y)
  substa f (x,y) = (substa f x, substa f y)

instance Subable a => Subable [a] where
  syms   = concatMap syms
  subst  = fmap . subst
  substf = fmap . substf
  substa = fmap . substa

instance Subable a => Subable (Maybe a) where
  syms   = concatMap syms . maybeToList
  subst  = fmap . subst
  substf = fmap . substf
  substa = fmap . substa


instance Subable a => Subable (M.HashMap k a) where
  syms   = syms . M.elems
  subst  = M.map . subst
  substf = M.map . substf
  substa = M.map . substa

subst1Except :: (Subable a) => [Symbol] -> a -> (Symbol, Expr) -> a
subst1Except xs z su@(x, _)
  | x `elem` xs = z
  | otherwise   = subst1 z su

substfExcept :: (Symbol -> Expr) -> [Symbol] -> Symbol -> Expr
substfExcept f xs y = if y `elem` xs then EVar y else f y

substExcept  :: Subst -> [Symbol] -> Subst
-- substExcept  (Su m) xs = Su (foldr M.delete m xs)
substExcept (Su xes) xs = Su $ M.filterWithKey (const . not . (`elem` xs)) xes

instance Subable Symbol where
  substa f                 = f
  substf f x               = subSymbol (Just (f x)) x
  subst su x               = subSymbol (Just $ appSubst su x) x -- subSymbol (M.lookup x s) x
  syms x                   = [x]

appSubst :: Subst -> Symbol -> Expr
appSubst (Su s) x = fromMaybe (EVar x) (M.lookup x s)

subSymbol :: Maybe Expr -> Symbol -> Symbol
subSymbol (Just (EVar y)) _ = y
subSymbol Nothing         x = x
subSymbol a               b = errorstar (printf "Cannot substitute symbol %s with expression %s" (showFix b) (showFix a))

captureAvoiding :: Symbol -> (Symbol -> Expr) -> Symbol -> Expr
captureAvoiding x f y = if y == x then EVar x else f y

instance Subable Expr where
  syms                     = exprSymbols
  substa f                 = substf (EVar . f)
  substf :: (Symbol -> Expr) -> Expr -> Expr
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
  substf f (PKVar k (Su su)) = PKVar k (Su $ M.map (substf f) su)
  substf _ (PAll _ _)      = errorstar "substf: FORALL"
  substf f (PExist xts e)  = PExist xts (substf f e)
  substf _  p              = p


  subst su (EApp f e)      = EApp (subst su f) (subst su e)
  subst su (ELam x e)      = ELam x (subst su' e) where su' = removeSubst su (fst x)
  subst su (ELet x e1 e2)  = ELet x (subst su e1) (subst su' e2) where su' = removeSubst su x
  subst su (ECoerc a t e)  = ECoerc a t (subst su e)
  subst su (ENeg e)        = ENeg (subst su e)
  subst su (EBin op e1 e2) = EBin op (subst su e1) (subst su e2)
  subst su (EIte p e1 e2)  = EIte (subst su p) (subst su e1) (subst su e2)
  subst su (ECst e so)     = ECst (subst su e) so
  subst su (EVar x)        = appSubst su x
  subst su (PAnd ps)       = PAnd $ map (subst su) ps
  subst su (POr  ps)       = POr  $ map (subst su) ps
  subst su (PNot p)        = PNot $ subst su p
  subst su (PImp p1 p2)    = PImp (subst su p1) (subst su p2)
  subst su (PIff p1 p2)    = PIff (subst su p1) (subst su p2)
  subst su (PAtom r e1 e2) = PAtom r (subst su e1) (subst su e2)
  subst su (PKVar k su')   = PKVar k $ su' `catSubst` su
  subst su (PAll bs p)
          | disjoint su bs = PAll bs $ subst su p --(substExcept su (fst <$> bs)) p
          | otherwise      = errorstar "subst: PAll (without disjoint binds)"
  subst su (PExist bs p)
          | disjoint su bs = PExist bs $ subst su p --(substExcept su (fst <$> bs)) p
          | otherwise      = errorstar ("subst: EXISTS (without disjoint binds)" ++ show (bs, su, p))
  subst _  p               = p

removeSubst :: Subst -> Symbol -> Subst
removeSubst (Su su) x = Su $ M.delete x su

-- | Rapier style capture-avoiding substitution
--
-- The scope set parameter must contain any symbols that are expected
-- to appear free in the result expression. Typically, this is the set of
-- symbols that are free in the range of the substitution, plus any symbols
-- that are already free in the input expression.
rapierSubstExpr :: S.HashSet Symbol -> Subst -> Expr -> Expr
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
    PKVar k su' -> PKVar k $ catSubstGo su' su
    PAll bs p ->
      let mfs = map (maybeFresh . fst) bs
          fs = rights mfs
          su' = List.foldl' (\su1 (x, x') -> extendSubst su1 x (EVar x')) su fs
          bs' = zip (map (either id snd) mfs) (map snd bs)
          s' = foldr (S.insert . fst) s bs'
       in
          PAll bs' $ go s' su' p
    PExist bs p ->
      let mfs = map (maybeFresh . fst) bs
          fs = rights mfs
          su' = List.foldl' (\su1 (x, x') -> extendSubst su1 x (EVar x')) su fs
          bs' = zip (map (either id snd) mfs) (map snd bs)
          s' = foldr (S.insert . fst) s bs'
       in
          PExist bs' $ go s' su' p
    p -> p
  where
    fresh :: Symbol -> Symbol
    fresh x = head $ dropWhile (`S.member` s) candidates
      where
        candidates = [ renameSubstSymbol x i | i <- [0..] ]

    maybeFresh x =
      if x `S.member` s then Right (x, fresh x) else Left x

    extendSubst :: Subst -> Symbol -> Expr -> Subst
    extendSubst (Su m) x e = Su $ M.insert x e m

    catSubstGo :: Subst -> Subst -> Subst
    catSubstGo (Su s1) su2@(Su s2) = Su $ M.union s1' s2
      where
        s1' = rapierSubstExpr s su2 <$> s1

disjoint :: Subst -> [(Symbol, Sort)] -> Bool
disjoint (Su su) bs = S.null $ suSyms `S.intersection` bsSyms
  where
    suSyms = S.fromList $ syms (M.elems su) ++ syms (M.keys su)
    bsSyms = S.fromList $ syms $ fst <$> bs

meetReft :: Reft -> Reft -> Reft
meetReft (Reft (v, ra)) (Reft (v', ra'))
  | v == v'          = Reft (v , pAnd [ra, ra'])
  | v == dummySymbol = Reft (v', pAnd [ra', ra `subst1`  (v , EVar v')])
  | otherwise        = Reft (v , pAnd [ra, ra' `subst1` (v', EVar v )])

instance Subable Reft where
  syms (Reft (v, ras))      = v : syms ras
  substa f (Reft (v, ras))  = Reft (f v, substa f ras)
  subst su (Reft (v, ras))  = Reft (v, subst (substExcept su [v]) ras)
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


exprSymbols :: Expr -> [Symbol]
exprSymbols = S.toList . exprSymbolsSet

instance Expression (Symbol, SortedReft) where
  expr (x, RR _ (Reft (v, r))) = subst1 (expr r) (v, EVar x)
