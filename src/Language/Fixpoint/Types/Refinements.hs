{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}

-- | This module has the types for representing terms in the refinement logic.

module Language.Fixpoint.Types.Refinements (

  -- * Representing Terms
    SymConst (..)
  , Constant (..)
  , Bop (..)
  , Brel (..)
  , ExprBV (..)
  , ExprV, Pred
  , Expr
  , pattern PTrue, pattern PTop, pattern PFalse, pattern EBot
  , pattern ETimes, pattern ERTimes, pattern EDiv, pattern ERDiv
  , pattern EEq
  , KVar (..)
  , Subst
  , SubstV (..)
  , KVarSubst
  , KVSub (..)
  , Reft
  , ReftV
  , ReftBV (..)
  , SortedReft (..)
  , TyVarSubst

  -- * Constructing Terms
  , eVar, elit
  , eProp
  , conj, pAnd, pOr, pIte, pAndNoDedup
  , (&.&), (|.|)
  , pExist
  , mkEApp
  , mkProp
  , intKvar
  , vv_

  -- * Generalizing Embedding with Typeclasses
  , Expression (..)
  , Predicate (..)
  -- * Constructors
  , reft                    -- "smart
  , trueSortedReft          -- trivial reft
  , trueReft, falseReft     -- trivial reft
  , exprReft                -- singleton: v == e
  , notExprReft             -- singleton: v /= e
  , uexprReft               -- singleton: v ~~ e
  , symbolReft              -- singleton: v == x
  , usymbolReft             -- singleton: v ~~ x
  , propReft                -- singleton: v <=> p
  , predReft                -- any pred : p
  , reftPred
  , reftBind
  , toKVarSubst

  -- * Predicates
  , isFunctionSortedReft, functionSort
  , isNonTrivial
  , isContraPred
  , isTautoPred
  , isTautoReft
  , isSingletonExpr
  , isSingletonReft
  , isFalse

  -- * Destructing
  , flattenRefas
  , conjuncts, concConjuncts
  , dropECst
  , eApps
  , eAppC
  , eCst
  , exprKVars
  , exprSymbolsSet
  , splitEApp
  , splitEAppThroughECst
  , splitPAnd
  , reftConjuncts
  , sortedReftSymbols
  , substSortInExpr
  , sortSubstInExpr
  , fromKVarSubst
  , isEmptyKVarSubst

  -- * Transforming
  , mapPredReft
  , onEverySubexpr
  , mapBindExpr
  , pprintReft
  , mapKVarSubst
  , mapBindKVarSubst
  , mapBindReft

  , debruijnIndex

  ) where

import           Prelude hiding ((<>))
import           Data.Bifunctor (first, second)
import qualified Data.Store as S
import           Data.Generics             (Data, gmapT, mkT, extT)
import           Data.Typeable             (Typeable)
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet
import           GHC.Generics              (Generic)

#if MIN_VERSION_base(4,20,0)
import           Data.List                 (partition)
#else
import           Data.List                 (foldl', partition)
#endif
import qualified Data.Set                  as Set
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.HashMap.Strict       as M
import           Control.DeepSeq
import           Data.Maybe                (isJust)
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Binders
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Spans
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ.Compat
import qualified Data.Binary as B
import           Data.Aeson

-- import           Text.Printf               (printf)


instance NFData KVar
instance NFData v => NFData (SubstV v)
instance NFData Constant
instance NFData SymConst
instance NFData Brel
instance NFData Bop
instance (NFData b, NFData v) => NFData (KVarSubst b v)
instance (NFData b, NFData v) => NFData (ExprBV b v)
instance NFData v => NFData (ReftV v)
instance NFData SortedReft

-- instance (Hashable k, Eq k, S.Store k, S.Store v) => S.Store (M.HashMap k v) where
  -- put = B.put . M.toList
  -- get = M.fromList <$> B.get

instance S.Store KVar
instance S.Store Subst
instance S.Store Constant
instance S.Store SymConst
instance S.Store Brel
instance S.Store Bop
instance S.Store (KVarSubst Symbol Symbol)
instance S.Store Expr
instance S.Store Reft
instance S.Store SortedReft

instance B.Binary SymConst
instance B.Binary Constant
instance B.Binary Bop
instance B.Binary Brel
instance B.Binary KVar
instance (Hashable a, Eq a, B.Binary a) => B.Binary (HashSet a) where
  put = B.put . HashSet.toList
  get = HashSet.fromList <$> B.get
instance (Hashable k, Eq k, B.Binary k, B.Binary v) => B.Binary (M.HashMap k v) where
  put = B.put . M.toList
  get = M.fromList <$> B.get

instance (B.Binary v, Hashable v) => B.Binary (SubstV v)
instance (B.Binary b, B.Binary v) => B.Binary (KVarSubst b v)
instance (B.Binary b, B.Binary v) => B.Binary (ExprBV b v)
instance (B.Binary b, B.Binary v) => B.Binary (ReftBV b v)


reftConjuncts :: Reft -> [Reft]
reftConjuncts (Reft (v, ra)) = [Reft (v, ra') | ra' <- ras']
  where
    ras'                     = if null ps then ks else conj ps : ks  -- see [NOTE:pAnd-SLOW]
    (ps, ks)                 = partition isConc (conjuncts ra)

isConc :: Expr -> Bool
isConc p = not (isKvar p)

concConjuncts :: Expr -> [Expr]
concConjuncts e = filter isConc (conjuncts e)

isKvar :: Expr -> Bool
isKvar (PKVar {}) = True
isKvar _           = False

--------------------------------------------------------------------------------
-- | Kvars ---------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype KVar = KV { kv :: Symbol }
               deriving (Eq, Ord, Data, Typeable, Generic, IsString, ToJSON, FromJSON)

instance ToJSONKey KVar

intKvar :: Integer -> KVar
intKvar = KV . intSymbol "k_"

instance Show KVar where
  show (KV x) = "$" ++ show x

instance Hashable KVar
instance Hashable Brel
instance Hashable Bop
instance Hashable SymConst
instance Hashable Constant
instance Hashable v => Hashable (SubstV v)
instance (Hashable b, Hashable v) => Hashable (KVarSubst b v)
instance (Hashable b, Hashable v) => Hashable (ExprBV b v)
instance (Hashable b, Hashable v) => Hashable (ReftBV b v)

--------------------------------------------------------------------------------
-- | Substitutions -------------------------------------------------------------
--------------------------------------------------------------------------------
type Subst = SubstV Symbol
newtype SubstV v = Su (M.HashMap v (ExprBV v v))
                deriving (Eq, Data, Ord, Typeable, Generic)

instance ToJSON Subst
instance FromJSON Subst

instance (Fixpoint v, Ord v, Hashable v, Show v) => Show (SubstV v) where
  show = showFix

instance (Ord v, Hashable v, Fixpoint v) => Fixpoint (SubstV v) where
  toFix (Su m) = toFix m

instance (Ord v, Hashable v, Fixpoint v) => PPrint (SubstV v) where
  pprintTidy _ = toFix

newtype KVarSubst b v = KSu [(b, ExprBV b v)]
  deriving (Eq, Ord, Data, Typeable, Generic, Functor, Foldable, Traversable)

fromKVarSubst :: Hashable b => KVarSubst b v -> M.HashMap b (ExprBV b v)
fromKVarSubst (KSu su) = M.fromList su

toKVarSubst :: M.HashMap b (ExprBV b v) -> KVarSubst b v
toKVarSubst = KSu . M.toList

mapKVarSubst :: (ExprBV b v -> ExprBV b v) -> KVarSubst b v -> KVarSubst b v
mapKVarSubst f (KSu su) = KSu $ fmap (fmap f) su

mapBindKVarSubst :: (Hashable b, Hashable b') => (b -> b') -> KVarSubst b v -> KVarSubst b' v
mapBindKVarSubst f = toKVarSubst . fmap (mapBindExpr f) . M.mapKeys f . fromKVarSubst

isEmptyKVarSubst :: KVarSubst b v -> Bool
isEmptyKVarSubst (KSu su) = null su

instance (Ord v, Fixpoint v, Ord b, Fixpoint b, Hashable b) => Show (KVarSubst b v) where
  show = showFix

instance (Ord v, Fixpoint v, Ord b, Fixpoint b, Hashable b) => Fixpoint (KVarSubst b v) where
  toFix = toFix . fromKVarSubst

instance (Ord v, Fixpoint v, Ord b, Fixpoint b, Hashable b) => PPrint (KVarSubst b v) where
  pprintTidy _ = toFix

data KVSub = KVS
  { ksuVV    :: Symbol
  , ksuSort  :: Sort
  , ksuKVar  :: KVar
  , ksuSubst :: KVarSubst Symbol Symbol
  , ksuTySub :: M.HashMap Symbol Sort  -- ^ Type variable substitution
  } deriving (Eq, Data, Typeable, Generic, Show)

instance PPrint KVSub where
  pprintTidy k ksu = pprintTidy k (ksuVV ksu, ksuKVar ksu, ksuSubst ksu)

--------------------------------------------------------------------------------
-- | Expressions ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Uninterpreted constants that are embedded as  "constant symbol : Str"

newtype SymConst = SL Text
                   deriving (Eq, Ord, Show, Data, Typeable, Generic, ToJSON, FromJSON)

data Constant = I !Integer
              | R !Double
              | L !Text !Sort
              deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Brel = Eq | Ne | Gt | Ge | Lt | Le | Ueq | Une
            deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Bop  = Plus | Minus | Times | Div | Mod | RTimes | RDiv
            deriving (Eq, Ord, Show, Data, Typeable, Generic)
            -- NOTE: For "Mod" 2nd expr should be a constant or a var *)

instance ToJSON Constant  where
instance ToJSON Brel      where
instance ToJSON Bop       where
instance ToJSON (KVarSubst Symbol Symbol) where
instance ToJSON Expr      where

instance FromJSON Constant  where
instance FromJSON Brel      where
instance FromJSON Bop       where
instance FromJSON (KVarSubst Symbol Symbol) where
instance FromJSON Expr      where


type Expr = ExprV Symbol
type ExprV v = ExprBV Symbol v
type TyVarSubst = M.HashMap Symbol Sort

data ExprBV b v
          = ESym !SymConst
          | ECon !Constant
          | EVar !v
          | EApp !(ExprBV b v) !(ExprBV b v)
          | ENeg !(ExprBV b v)
          | EBin !Bop !(ExprBV b v) !(ExprBV b v)
          | ELet !b !(ExprBV b v) !(ExprBV b v)
          | EIte !(ExprBV b v) !(ExprBV b v) !(ExprBV b v)
          | ECst !(ExprBV b v) !Sort
          | ELam !(b, Sort)   !(ExprBV b v)
          | ETApp !(ExprBV b v) !Sort
          | ETAbs !(ExprBV b v) !b
          | PAnd   ![ExprBV b v]
          | POr    ![ExprBV b v]
          | PNot   !(ExprBV b v)
          | PImp   !(ExprBV b v) !(ExprBV b v)
          | PIff   !(ExprBV b v) !(ExprBV b v)
          | PAtom  !Brel  !(ExprBV b v) !(ExprBV b v)
            -- | In @PKVar k su tsu@, @k@ is the KVar, @su@ is the substitution
            -- for that KVar, and @tsu@ indicates how to instantiate type
            -- variables that could appear in the KVar solution.
          | PKVar  !KVar !TyVarSubst !(KVarSubst b v)
          | PAll   ![(b, Sort)] !(ExprBV b v)
          | PExist ![(b, Sort)] !(ExprBV b v)
          | ECoerc !Sort !Sort !(ExprBV b v)
          deriving (Eq, Show, Ord, Data, Typeable, Generic, Functor, Foldable, Traversable)

onEverySubexpr :: (Expr -> Expr) -> Expr -> Expr
onEverySubexpr = everywhereOnA

-- | Like 'Data.Generics.everywhere' but only traverses the nodes
-- of type @a@ or @[a]@.
everywhereOnA :: forall a. Data a => (a -> a) -> a -> a
everywhereOnA f = go
  where
    go :: a -> a
    go = f . gmapT (mkT go `extT` map go)

type Pred = Expr

pattern PTrue :: ExprBV b v
pattern PTrue = PAnd []

pattern PTop :: ExprBV b v
pattern PTop = PAnd []

pattern PFalse :: ExprBV b v
pattern PFalse = POr  []

pattern EBot :: ExprBV b v
pattern EBot = POr  []

pattern EEq :: ExprBV b v -> ExprBV b v -> ExprBV b v
pattern EEq e1 e2 = PAtom Eq    e1 e2

pattern ETimes :: ExprBV b v -> ExprBV b v -> ExprBV b v
pattern ETimes e1 e2 = EBin Times  e1 e2

pattern ERTimes :: ExprBV b v -> ExprBV b v -> ExprBV b v
pattern ERTimes e1 e2 = EBin RTimes e1 e2

pattern EDiv :: ExprBV b v -> ExprBV b v -> ExprBV b v
pattern EDiv e1 e2 = EBin Div    e1 e2

pattern ERDiv :: ExprBV b v -> ExprBV b v -> ExprBV b v
pattern ERDiv e1 e2 = EBin RDiv   e1 e2

mapBindExpr :: (Hashable b, Hashable b') => (b -> b') -> ExprBV b v -> ExprBV b' v
mapBindExpr f = go
  where
    go (ESym c) = ESym c
    go (ECon c) = ECon c
    go (EVar v) = EVar v
    go (EApp e1 e2) = EApp (go e1) (go e2)
    go (ENeg e) = ENeg (go e)
    go (EBin op e1 e2) = EBin op (go e1) (go e2)
    go (ELet b e1 e2) = ELet (f b) (go e1) (go e2)
    go (EIte e1 e2 e3) = EIte (go e1) (go e2) (go e3)
    go (ECst e s) = ECst (go e) s
    go (ELam (b, s) e) = ELam (f b, s) (go e)
    go (ETApp e s) = ETApp (go e) s
    go (ETAbs e b) = ETAbs (go e) (f b)
    go (PAnd es) = PAnd (go <$> es)
    go (POr es) = POr (go <$> es)
    go (PNot e) = PNot (go e)
    go (PImp e1 e2) = PImp (go e1) (go e2)
    go (PIff e1 e2) = PIff (go e1) (go e2)
    go (PAtom rel e1 e2) = PAtom rel (go e1) (go e2)
    go (PKVar k tsu su) = PKVar k tsu (mapBindKVarSubst f su)
    go (PAll bs e) = PAll (first f <$> bs) (go e)
    go (PExist bs e) = PExist (first f <$> bs) (go e)
    go (ECoerc s1 s2 e) = ECoerc s1 s2 (go e)

exprSymbolsSet :: (Eq v, Hashable v) => ExprBV v v -> HashSet v
exprSymbolsSet = go
  where
    gos es                = HashSet.unions (go <$> es)
    go (EVar x)           = HashSet.singleton x
    go (EApp f e)         = gos [f, e]
    go (ELam (x,_) e)     = HashSet.delete x (go e)
    go (ECoerc _ _ e)     = go e
    go (ENeg e)           = go e
    go (EBin _ e1 e2)     = gos [e1, e2]
    go (ELet x e1 e2)     = HashSet.union (go e1) (HashSet.delete x $ go e2)
    go (EIte p e1 e2)     = gos [p, e1, e2]
    go (ECst e _)         = go e
    go (PAnd ps)          = gos ps
    go (POr ps)           = gos ps
    go (PNot p)           = go p
    go (PIff p1 p2)       = gos [p1, p2]
    go (PImp p1 p2)       = gos [p1, p2]
    go (PAtom _ e1 e2)    = gos [e1, e2]
    go (PKVar _ _ su)       = HashSet.unions $ map exprSymbolsSet (M.elems $ fromKVarSubst su)
    go (PAll xts p)       = go p `HashSet.difference` HashSet.fromList (fst <$> xts)
    go (PExist xts p)     = go p `HashSet.difference` HashSet.fromList (fst <$> xts)
    go _                  = HashSet.empty

substSortInExpr :: (Symbol -> Sort) -> Expr -> Expr
substSortInExpr f = onEverySubexpr go
  where
    go = \case
      ELam (x, t) e -> ELam (x, substSort f t) e
      PAll xts e -> PAll (second (substSort f) <$> xts) e
      PExist xts e -> PExist (second (substSort f) <$> xts) e
      ECst e t -> ECst e (substSort f t)
      ECoerc t0 t1 e -> ECoerc (substSort f t0) (substSort f t1) e
      e -> e


sortSubstInExpr :: SortSubst -> Expr -> Expr
sortSubstInExpr f = onEverySubexpr go
  where
    go = \case
      ELam (x, t) e -> ELam (x, sortSubst f t) e
      PAll xts e -> PAll (second (sortSubst f) <$> xts) e
      PExist xts e -> PExist (second (sortSubst f) <$> xts) e
      ECst e t -> ECst e (sortSubst f t)
      ECoerc t0 t1 e -> ECoerc (sortSubst f t0) (sortSubst f t1) e
      e -> e

exprKVars :: Expr -> HashMap KVar [KVarSubst Symbol Symbol]
exprKVars = go
  where
    gos es                = HashMap.unions (go <$> es)
    go (EVar _)           = HashMap.empty
    go (EApp f e)         = gos [f, e]
    go (ELam _ e)     = go e
    go (ECoerc _ _ e)     = go e
    go (ENeg e)           = go e
    go (EBin _ e1 e2)     = gos [e1, e2]
    go (ELet _ e1 e2)     = gos [e1, e2]
    go (EIte p e1 e2)     = gos [p, e1, e2]
    go (ECst e _)         = go e
    go (PAnd ps)          = gos ps
    go (POr ps)           = gos ps
    go (PNot p)           = go p
    go (PIff p1 p2)       = gos [p1, p2]
    go (PImp p1 p2)       = gos [p1, p2]
    go (PAtom _ e1 e2)    = gos [e1, e2]
    go (PKVar k _ su) =
      HashMap.insertWith (++) k [su] $ HashMap.unions $ map exprKVars (M.elems $ fromKVarSubst su)
    go (PAll _xts p)       = go p
    go (PExist _xts p)     = go p
    go _                  = HashMap.empty

mkEApp :: Located v -> [ExprBV b v] -> ExprBV b v
mkEApp = eApps . EVar . val

eApps :: ExprBV b v -> [ExprBV b v] -> ExprBV b v
eApps f es  = foldl' EApp f es

splitEApp :: ExprBV b v -> (ExprBV b v, [ExprBV b v])
splitEApp = go []
  where
    go acc (EApp f e) = go (e:acc) f
    go acc e          = (e, acc)

splitEAppThroughECst :: Expr -> (Expr, [Expr])
splitEAppThroughECst = go []
  where
    go acc (dropECst -> (EApp f e)) = go (e:acc) f
    go acc e                        = (e, acc)

dropECst :: Expr -> Expr
dropECst e = case e of
  ECst e' _ -> dropECst e'
  _         -> e

splitPAnd :: Expr -> [Expr]
splitPAnd (PAnd es) = concatMap splitPAnd es
splitPAnd e         = [e]

eAppC :: Sort -> Expr -> Expr -> Expr
eAppC s e1 e2 = eCst (EApp e1 e2) s

-- | Eliminates redundant casts
eCst :: Expr -> Sort -> Expr
eCst e s = ECst (dropECst e) s

--------------------------------------------------------------------------------
debruijnIndex :: Expr -> Int
debruijnIndex = go
  where
    go (ELam _ e)      = 1 + go e
    go (ECst e _)      = go e
    go (EApp e1 e2)    = go e1 + go e2
    go (ESym _)        = 1
    go (ECon _)        = 1
    go (EVar _)        = 1
    go (ENeg e)        = go e
    go (EBin _ e1 e2)  = go e1 + go e2
    go (ELet _ e1 e2)  = 1 + go e1 + go e2
    go (EIte e e1 e2)  = go e + go e1 + go e2
    go (ETAbs e _)     = go e
    go (ETApp e _)     = go e
    go (PAnd es)       = foldl' (\n e -> n + go e) 0 es
    go (POr es)        = foldl' (\n e -> n + go e) 0 es
    go (PNot e)        = go e
    go (PImp e1 e2)    = go e1 + go e2
    go (PIff e1 e2)    = go e1 + go e2
    go (PAtom _ e1 e2) = go e1 + go e2
    go (PAll _ e)      = go e
    go (PExist _ e)    = go e
    go (PKVar {})        = 1
    go (ECoerc _ _ e)  = go e

type Reft = ReftV Symbol
type ReftV v = ReftBV Symbol v

-- | Refinement of @v@ satisfying a predicate
--   e.g. in '{x: _ | e }' x is the @Symbol@ and e the @ExprV v@
newtype ReftBV b v = Reft (b, ExprBV b v)
    deriving (Eq, Ord, Data, Typeable, Generic, Functor, Foldable, Traversable)

mapBindReft :: (Hashable b, Hashable b') => (b -> b') -> ReftBV b v -> ReftBV b' v
mapBindReft f (Reft (b, e)) = Reft (f b, mapBindExpr f e)

data SortedReft = RR { sr_sort :: !Sort, sr_reft :: !Reft }
                  deriving (Eq, Ord, Data, Typeable, Generic)

instance Hashable SortedReft

sortedReftSymbols :: SortedReft -> HashSet Symbol
sortedReftSymbols sr =
  HashSet.union
    (sortSymbols $ sr_sort sr)
    (exprSymbolsSet $ reftPred $ sr_reft sr)

elit :: Located Symbol -> Sort -> Expr
elit l s = ECon $ L (symbolText $ val l) s

instance Fixpoint Constant where
  toFix (I i)   = toFix i
  toFix (R i)   = toFix i
  toFix (L s t) = parens $ text "lit" <+> text "\"" <-> toFix s <-> text "\"" <+> toFix t

--------------------------------------------------------------------------------
-- | String Constants ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Replace all symbol-representations-of-string-literals with string-literal
--   Used to transform parsed output from fixpoint back into fq.

instance Symbolic SymConst where
  symbol = encodeSymConst

encodeSymConst        :: SymConst -> Symbol
encodeSymConst (SL s) = litSymbol $ symbol s

instance Fixpoint SymConst where
  toFix (SL t) = text (show t)

instance Fixpoint KVar where
  toFix (KV k) = text "$" <-> toFix k

instance Fixpoint Brel where
  toFix Eq  = text "="
  toFix Ne  = text "!="
  toFix Ueq = text "~~"
  toFix Une = text "!~"
  toFix Gt  = text ">"
  toFix Ge  = text ">="
  toFix Lt  = text "<"
  toFix Le  = text "<="

instance Fixpoint Bop where
  toFix Plus   = text "+"
  toFix Minus  = text "-"
  toFix RTimes = text "*."
  toFix Times  = text "*"
  toFix Div    = text "/"
  toFix RDiv   = text "/."
  toFix Mod    = text "mod"

instance (Ord b, Fixpoint b, Hashable b, Ord v, Fixpoint v) => Fixpoint (ExprBV b v) where
  toFix (ESym c)       = toFix c
  toFix (ECon c)       = toFix c
  toFix (EVar s)       = toFix s
  toFix e@(EApp _ _)   = parens $ hcat $ punctuate " " $ toFix <$> (f:es) where (f, es) = splitEApp e
  toFix (ENeg e)       = parens $ text "-"  <+> parens (toFix e)
  toFix (EBin o e1 e2) = parens $ sep [toFix e1  <+> toFix o, nest 2 (toFix e2)]
  toFix (ELet x e1 e2) = parens $ sep [text "let" <+> toFix x <+> text "=" <+> toFix e1 <+> text "in", nest 2 (toFix e2)]
  toFix (EIte p e1 e2) = parens $ sep [text "if" <+> toFix p <+> text "then", nest 2 (toFix e1), text "else", nest 2 (toFix e2)]
  -- toFix (ECst e _so)   = toFix e
  toFix (ECst e so)    = parens $ toFix e   <+> text " : " <+> toFix so
  -- toFix (EBot)         = text "_|_"
  -- toFix PTop           = text "???"
  toFix PTrue          = text "true"
  toFix PFalse         = text "false"
  toFix (PNot p)       = parens $ text "~" <+> parens (toFix p)
  toFix (PImp p1 p2)   = parens $ toFix p1 <+> text "=>" <+> toFix p2
  toFix (PIff p1 p2)   = parens $ toFix p1 <+> text "<=>" <+> toFix p2
  toFix (PAnd ps)      = text "&&" <+> toFix ps
  toFix (POr  ps)      = text "||" <+> toFix ps
  toFix (PAtom r e1 e2)  = parens $ sep [ toFix e1 <+> toFix r, nest 2 (toFix e2)]
  toFix (PKVar k tsu su)   = toFix k <-> toFixTySub tsu <-> toFix su
  toFix (PAll xts p)     = parens $ "forall" <+> (toFix xts
                                        $+$ ("." <+> toFix p))
  toFix (PExist xts p)   = parens $ "exists" <+> (toFix xts
                                        $+$ ("." <+> toFix p))
  toFix (ETApp e s)      = text "tapp" <+> toFix e <+> toFix s
  toFix (ETAbs e s)      = text "tabs" <+> toFix e <+> toFix s
  toFix (ECoerc a t e)   = parens (text "coerce" <+> toFix a <+> text "~" <+> toFix t <+> text "in" <+> toFix e)
  toFix (ELam (x,s) e)   = parens (char '\\' <+> toFix x <+> ":" <+> toFix s <+> "->" <+> toFix e)

  simplify = simplifyExprDefault

-- | Serialize a type-variable substitution for PKVar in .fq files.
-- An empty substitution is rendered as @[@]@, and a non-empty one as
-- @[\@sym:=sort;...]@.
toFixTySub :: M.HashMap Symbol Sort -> Doc
toFixTySub tsu
  | M.null tsu = empty
  | otherwise  = brackets (text "@" <->  tyPairs)
  where
    tyPairs = hcat $ punctuate (text ";") (toFixTyPair <$> hashMapToAscList tsu)
    toFixTyPair (s, srt) = toFix s <-> text ":=" <-> toFix srt

simplifyExprDefault :: (Ord b, Ord v) => ExprBV b v -> ExprBV b v
simplifyExprDefault = simplifyExpr (Set.toList . Set.fromList)

simplifyExpr :: (Eq b, Eq v) => ([ExprBV b v] -> [ExprBV b v]) -> ExprBV b v -> ExprBV b v
simplifyExpr dedup = go
  where
    go (POr  [])     = PFalse
    go (POr  [p])    = go p
    go (PNot p) =
      let sp = go p
       in case sp of
            PNot e -> e
            _ -> PNot sp
    -- XXX: Do not simplify PImp until PLE can handle it
    -- https://github.com/ucsd-progsys/liquid-fixpoint/issues/475
    -- go (PImp p q) =
    --   let sq = go q
    --    in if sq == PTrue then PTrue
    --       else if sq == PFalse then go (PNot p)
    --       else PImp (go p) sq
    go (PIff p q)    =
      let sp = go p
          sq = go q
       in if sp == sq then PTrue
          else if sp == PTrue then sq
          else if sq == PTrue then sp
          else if sp == PFalse then PNot sq
          else if sq == PFalse then PNot sp
          else PIff sp sq

    go (PAnd ps)
      | any isContraPred ps = PFalse
                           -- Note: Performance of some tests is very sensitive to this code. See #480
      | otherwise           = simplPAnd . dedup . flattenRefas . filter (not . isTautoPred) $ map go ps
      where
        simplPAnd [] = PTrue
        simplPAnd [p] = p
        simplPAnd xs = PAnd xs

    go (POr  ps)
      | any isTautoPred ps = PTrue
      | otherwise          = POr  $ filter (not . isContraPred) $ map go ps

    go p
      | isContraPred p     = PFalse
      | isTautoPred  p     = PTrue
      | otherwise          = p

isContraPred   :: (Eq b, Eq v) => ExprBV b v -> Bool
isContraPred z = eqC z || (z `elem` contras)
  where
    contras    = [PFalse]

    eqC (PAtom Eq (ECon x) (ECon y))
               = x /= y
    eqC (PAtom Ueq (ECon x) (ECon y))
               = x /= y
    eqC (PAtom Ne x y)
               = x == y
    eqC (PAtom Une x y)
               = x == y
    eqC _      = False

isTautoPred   :: (Eq b, Eq v) => ExprBV b v -> Bool
isTautoPred z  = z == PTop || z == PTrue || eqT z
  where
    eqT (PAnd [])
               = True
    eqT (PAtom Le x y)
               = x == y
    eqT (PAtom Ge x y)
               = x == y
    eqT (PAtom Eq x y)
               = x == y
    eqT (PAtom Ueq x y)
               = x == y
    eqT (PAtom Ne (ECon x) (ECon y))
               = x /= y
    eqT (PAtom Une (ECon x) (ECon y))
               = x /= y
    eqT _      = False

isEq  :: Brel -> Bool
isEq r          = r == Eq || r == Ueq

instance PPrint Constant where
  pprintTidy _ = toFix

instance PPrint Brel where
  pprintTidy _ Eq = "=="
  pprintTidy _ Ne = "/="
  pprintTidy _ r  = toFix r

instance PPrint Bop where
  pprintTidy _  = toFix

instance PPrint KVar where
  pprintTidy _ (KV x) = text "$" <-> pprint x

instance PPrint SymConst where
  pprintTidy _ (SL x) = doubleQuotes $ text $ T.unpack x

-- | Wrap the enclosed 'Doc' in parentheses only if the condition holds.
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- NOTE: The following Expr and Pred printers use pprintPrec to print
-- expressions with minimal parenthesization. The precedence rules are somewhat
-- fragile, and it would be nice to have them directly tied to the parser, but
-- the general idea is (from lowest to highest precedence):
--
-- 1 - if-then-else
-- 2 - => and <=>
-- 3 - && and ||
-- 4 - ==, !=, <, <=, >, >=
-- 5 - mod
-- 6 - + and -
-- 7 - * and /
-- 8 - function application
--
-- Each printer `p` checks whether the precedence of the context is greater than
-- its own precedence. If so, the printer wraps itself in parentheses. Then it
-- sets the contextual precedence for recursive printer invocations to
-- (prec p + 1).

opPrec :: Bop -> Int
opPrec Mod    = 5
opPrec Plus   = 6
opPrec Minus  = 6
opPrec Times  = 7
opPrec RTimes = 7
opPrec Div    = 7
opPrec RDiv   = 7

instance (Ord b, Fixpoint b, Hashable b, PPrint b, Ord v, Fixpoint v, PPrint v) => PPrint (ExprBV b v) where
  pprintPrec _ k (ESym c)        = pprintTidy k c
  pprintPrec _ k (ECon c)        = pprintTidy k c
  pprintPrec _ k (EVar s)        = pprintTidy k s
  -- pprintPrec _ (EBot)          = text "_|_"
  pprintPrec z k (ENeg e)        = parensIf (z > zn) $
                                   "-" <-> pprintPrec (zn + 1) k e
    where zn = 2
  pprintPrec z k (EApp f es)     = parensIf (z > za) $
                                   pprintPrec za k f <+> pprintPrec (za+1) k es
    where za = 8
  pprintPrec z k (EBin o e1 e2)  = parensIf (z > zo) $
                                   pprintPrec (zo+1) k e1 <+>
                                   pprintTidy k o         <+>
                                   pprintPrec (zo+1) k e2
    where zo = opPrec o
  pprintPrec _ k (ELet x e1 e2)  = parens
                                   "let"  <+> toFix x <+> "=" <+> pprintTidy  k e1  <+>
                                   "in"   <+> pprintTidy k e2

  pprintPrec z k (EIte p e1 e2)  = parensIf (z > zi) $
                                   "if"   <+> pprintPrec (zi+1) k p  <+>
                                   "then" <+> pprintPrec (zi+1) k e1 <+>
                                   "else" <+> pprintPrec (zi+1) k e2
    where zi = 1

  -- RJ: DO NOT DELETE!
  pprintPrec _ k (ECst e so)     = parens $ pprint e <+> ":" <+> {- const (text "...") -} pprintTidy k so
  -- pprintPrec z k (ECst e _)      = pprintPrec z k e
  pprintPrec _ _ PTrue           = trueD
  pprintPrec _ _ PFalse          = falseD
  pprintPrec z k (PNot p)        = parensIf (z > zn) $
                                   "not" <+> pprintPrec (zn+1) k p
    where zn = 8
  pprintPrec z k (PImp p1 p2)    = parensIf (z > zi) $
                                   pprintPrec (zi+1) k p1 <+>
                                   "=>"                     <+>
                                   pprintPrec (zi+1) k p2
    where zi = 2
  pprintPrec z k (PIff p1 p2)    = parensIf (z > zi) $
                                   pprintPrec (zi+1) k p1 <+>
                                   "<=>"                    <+>
                                   pprintPrec (zi+1) k p2
    where zi = 2
  pprintPrec z k (PAnd ps)       = parensIf (z > za) $
                                   pprintBin (za + 1) k trueD andD ps
    where za = 3
  pprintPrec z k (POr  ps)       = parensIf (z > zo) $
                                   pprintBin (zo + 1) k falseD orD ps
    where zo = 3
  pprintPrec z k (PAtom r e1 e2) = parensIf (z > za) $
                                   pprintPrec (za+1) k e1 <+>
                                   pprintTidy k r         <+>
                                   pprintPrec (za+1) k e2
    where za = 4
  pprintPrec z k (PAll xts p)    = parensIf (z > 0) $ pprintQuant k "forall" xts p
  pprintPrec z k (PExist xts p)  = parensIf (z > 0) $ pprintQuant k "exists" xts p
  pprintPrec _ k (ELam (x,t) e)  = "lam" <+> toFix x <+> ":" <+> toFix t <+> text "." <+> pprintTidy k e
  pprintPrec _ k (ECoerc a t e)  = parens $ "coerce" <+> toFix a <+> "~" <+> toFix t <+> text "in" <+> pprintTidy k e
  pprintPrec _ _ p@PKVar{}    = toFix p
  pprintPrec _ _ (ETApp e s)     = "ETApp" <+> toFix e <+> toFix s
  pprintPrec _ _ (ETAbs e s)     = "ETAbs" <+> toFix e <+> toFix s

pprintQuant
  :: (Ord b, Fixpoint b, Hashable b, PPrint b, Ord v, Fixpoint v, PPrint v)
  => Tidy -> Doc -> [(b, Sort)] -> ExprBV b v -> Doc
pprintQuant k d xts p = (d <+> pprintTidy k xts)
                        $+$
                        ("  ." <+> pprintTidy k p)

trueD, falseD, andD, orD :: Doc
trueD  = "true"
falseD = "false"
andD   = "&&"
orD    = "||"

pprintBin :: (PPrint a) => Int -> Tidy -> Doc -> Doc -> [a] -> Doc
pprintBin _ _ b _ [] = b
pprintBin z k _ o xs = vIntersperse o $ pprintPrec z k <$> xs

vIntersperse :: Doc -> [Doc] -> Doc
vIntersperse _ []     = empty
vIntersperse _ [d]    = d
vIntersperse s (d:ds) = vcat (d : ((s <+>) <$> ds))

pprintReft :: (PPrint v, Ord v, Fixpoint v) => Tidy -> ReftV v -> Doc
pprintReft k (Reft (_,ra)) = pprintBin z k trueD andD flat
  where
    flat = flattenRefas [ra]
    z    = if length flat > 1 then 3 else 0

------------------------------------------------------------------------
-- | Generalizing Symbol, Expression, Predicate into Classes -----------
------------------------------------------------------------------------

-- | Values that can be viewed as Constants

-- | Values that can be viewed as Expressions

class Expression a where
  expr   :: a -> Expr

-- | Values that can be viewed as Predicates

class Predicate a where
  prop   :: a -> Expr

instance Expression SortedReft where
  expr (RR _ r) = expr r

instance Expression Reft where
  expr (Reft(_, e)) = e

instance Expression Expr where
  expr = id

-- | The symbol may be an encoding of a SymConst.

instance Expression Symbol where
  expr s = eVar s

instance Expression Text where
  expr = ESym . SL

instance Expression Integer where
  expr = ECon . I

instance Expression Int where
  expr = expr . toInteger

instance Predicate Symbol where
  prop = eProp

instance Predicate Expr where
  prop = id

instance Predicate Bool where
  prop True  = PTrue
  prop False = PFalse

instance Expression a => Expression (Located a) where
  expr   = expr . val

eVar ::  Symbolic a => a -> Expr
eVar = EVar . symbol

eProp ::  Symbolic a => a -> Expr
eProp = mkProp . eVar

isSingletonExpr :: Symbol -> Expr -> Maybe Expr
isSingletonExpr v (PAtom r e1 e2)
  | e1 == EVar v && isEq r = Just e2
  | e2 == EVar v && isEq r = Just e1
isSingletonExpr v (PIff e1 e2)
  | e1 == EVar v           = Just e2
  | e2 == EVar v           = Just e1
isSingletonExpr _ _        = Nothing

-- | 'conj' is a fast version of 'pAnd' needed for the ebind tests
conj :: [Pred] -> Pred
conj []  = PTrue
conj [p] = p
conj ps  = PAnd ps

-- | [NOTE: pAnd-SLOW] 'pAnd' and 'pOr' are super slow as they go inside the predicates;
--   so they SHOULD NOT be used inside the solver loop. Instead, use 'conj' which ensures
--   some basic things but is faster.

pAnd, pOr     :: (Ord b, Hashable b, Ord v) => ListNE (ExprBV b v) -> ExprBV b v
pAnd          = simplifyExprDefault . PAnd

pAndNoDedup :: ListNE Pred -> Pred
pAndNoDedup = simplifyExpr id . PAnd

pOr           = simplifyExprDefault . POr

infixl 9 &.&
(&.&) :: Pred -> Pred -> Pred
(&.&) p q = pAnd [p, q]

infixl 9 |.|
(|.|) :: Pred -> Pred -> Pred
(|.|) p q = pOr [p, q]

pIte :: (Fixpoint b, Ord b, Hashable b, Fixpoint v, Ord v) => ExprBV b v -> ExprBV b v -> ExprBV b v -> ExprBV b v
pIte p1 p2 p3 = pAnd [p1 `PImp` p2, PNot p1 `PImp` p3]

pExist :: [(b, Sort)] -> ExprBV b v -> ExprBV b v
pExist []  p = p
pExist xts p = PExist xts p

mkProp :: Expr -> Pred
mkProp = id

--------------------------------------------------------------------------------
-- | Predicates ----------------------------------------------------------------
--------------------------------------------------------------------------------

isSingletonReft :: Reft -> Maybe Expr
isSingletonReft (Reft (v, ra)) = firstMaybe (isSingletonExpr v) $ conjuncts ra

relReft :: (Expression a) => Brel -> a -> Reft
relReft r e   = Reft (vv_, PAtom r (eVar vv_)  (expr e))

exprReft, notExprReft, uexprReft ::  (Expression a) => a -> Reft
exprReft      = relReft Eq
notExprReft   = relReft Ne
uexprReft     = relReft Ueq

propReft      ::  (Predicate a) => a -> Reft
propReft p    = Reft (vv_, PIff (eProp vv_) (prop p))

predReft      :: (Predicate a) => a -> Reft
predReft p    = Reft (vv_, prop p)

reft :: Symbol -> ExprV v -> ReftV v
reft v p = Reft (v, p)

mapPredReft :: (Expr -> Expr) -> Reft -> Reft
mapPredReft f (Reft (v, p)) = Reft (v, f p)

---------------------------------------------------------------
-- | Refinements ----------------------------------------------
---------------------------------------------------------------

isFunctionSortedReft :: SortedReft -> Bool
isFunctionSortedReft = isJust . functionSort . sr_sort

isNonTrivial :: SortedReft -> Bool
isNonTrivial = not . isTautoReft . sr_reft

isTautoReft :: (Eq b, Eq v) => ReftBV b v -> Bool
isTautoReft = all isTautoPred . conjuncts . reftPred

reftPred :: ReftBV b v -> ExprBV b v
reftPred (Reft (_, p)) = p

reftBind :: ReftBV b v -> b
reftBind (Reft (x, _)) = x

------------------------------------------------------------
-- | Generally Useful Refinements --------------------------
------------------------------------------------------------

symbolReft    :: (Symbolic a) => a -> Reft
symbolReft    = exprReft . eVar

usymbolReft   :: (Symbolic a) => a -> Reft
usymbolReft   = uexprReft . eVar

vv_ :: Symbol
vv_ = vv Nothing

trueSortedReft :: Sort -> SortedReft
trueSortedReft = (`RR` trueReft)

trueReft, falseReft :: Binder b => ReftBV b v
trueReft  = Reft (wildcard, PTrue)
falseReft = Reft (wildcard, PFalse)

flattenRefas :: [ExprBV b v] -> [ExprBV b v]
flattenRefas        = flatP []
  where
    flatP acc (PAnd ps:xs) = flatP (flatP acc xs) ps
    flatP acc (p:xs)       = p : flatP acc xs
    flatP acc []           = acc

conjuncts :: (Eq b, Eq v) => ExprBV b v -> [ExprBV b v]
conjuncts (PAnd ps) = concatMap conjuncts ps
conjuncts p
  | isTautoPred p   = []
  | otherwise       = [p]


-------------------------------------------------------------------------
-- | TODO: This doesn't seem to merit a TC ------------------------------
-------------------------------------------------------------------------

class Falseable a where
  isFalse :: a -> Bool

instance Falseable (ExprBV b v) where
  isFalse PFalse = True
  isFalse _      = False

instance Falseable (ReftBV b v) where
  isFalse (Reft (_, ra)) = isFalse ra


instance Fixpoint Doc where
  toFix = id
