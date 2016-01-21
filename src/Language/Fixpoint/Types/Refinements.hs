{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE PatternSynonyms            #-}

-- | This module contains the data types for representing terms in the
--   refinement logic; currently split into @Expr@ and @Pred@ but which
--   will be unified.

module Language.Fixpoint.Types.Refinements (

  -- * Representing Terms
    symConst
  , Constant (..)
  , Bop (..)
  , SReft(..), BReft, SortedReft(..)
  , Brel (..)
  , Expr, BExpr, SExpr (..)
  , Var (..), VInfo(..)
  , KVar (..)
  , SSubst (..), BSubst, Subst 
  , Reft

  -- * Constructing Terms
  , eVar, elit
  , eProp
  , pAnd, pOr, pIte
  , isTautoPred

  -- * Generalizing Embedding with Typeclasses
  , Expression (..), exprSym
  , pattern PTrue, pattern PTop, pattern PFalse, pattern EBot
  , Predicate (..)
  , Subable (..)
  , Reftable (..)

  -- * Constructing Refinements
  , reft                    -- "smart
  , trueReft, falseReft     -- trivial reft
  , exprReft                -- singleton: v == e
  , notExprReft             -- singleton: v /= e
  , uexprReft               -- singleton: v ~~ e
--   , symbolReft              -- singleton: v == x
--   , usymbolReft             -- singleton: v ~~ x
  , propReft                -- singleton: Prop(v) <=> p
  , predReft                -- any pred : p
  , reftPred, reftBind
  , isFunctionReft
  , isNonTrivial
  , isSingletonReft
  , isEVar
  , isFalse
  , flattenRefas, conjuncts
  , mapPredReft
  , pprintReft
  , reftConjuncts
  , intKvar
  , vv_
  , mkEApp, bmkEApp, splitArgs 
  , makeVar, makeVarWithLoc, locSymbolVar, makeSMTVar
  , reftparam, reftSort
  , mapVarSymbol, mapVarSort  
  ) where

import qualified Data.Binary as B
import           Data.Generics             (Data)
import           Data.Typeable             (Typeable)
import           Data.Hashable
import           GHC.Generics              (Generic)
import           Data.List                 (partition) -- , foldl', sort, sortBy)
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Control.DeepSeq
-- import           Text.Printf               (printf)
-- import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.PrettyPrint
-- import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Spans
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Misc
-- import           Text.Parsec.Pos
import           Text.PrettyPrint.HughesPJ 
-- import           Data.Array                hiding (indices)
import qualified Data.HashMap.Strict       as M
-- import qualified Data.HashSet              as S

import qualified Data.Semigroup as SG

instance NFData KVar
instance NFData Subst
instance NFData Constant
instance NFData Brel
instance NFData Bop
instance NFData Expr
instance NFData SortedReft
instance NFData Reft
instance NFData Var
instance NFData VInfo

instance (Hashable k, Eq k, B.Binary k, B.Binary v) => B.Binary (M.HashMap k v) where
  put = B.put . M.toList
  get = M.fromList <$> B.get

instance B.Binary KVar
instance B.Binary Subst
instance B.Binary Constant
instance B.Binary Brel
instance B.Binary Bop
instance B.Binary Expr
instance B.Binary Reft
instance B.Binary SortedReft
instance B.Binary Var
instance B.Binary VInfo



reftConjuncts :: (Eq s, Eq t, Fixpoint t, Ord t) => SReft s t -> [SReft s t]
reftConjuncts (Reft (v, ra)) = [Reft (v, ra') | ra' <- ras']
  where
    ras'                     = if null ps then ks else ((pAnd ps) : ks)
    (ks, ps)                 = partition isKvar $ refaConjuncts ra

isKvar :: SExpr s t -> Bool
isKvar (PKVar _ _) = True
isKvar _           = False

refaConjuncts :: (Eq s, Eq t) => SExpr s t -> [SExpr s t]
refaConjuncts p              = [p' | p' <- conjuncts p, not $ isTautoPred p']


--------------------------------------------------------------------------------
-- | Kvars ---------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype KVar = KV { kv :: Symbol }
               deriving (Eq, Ord, Data, Typeable, Generic, IsString)


intKvar :: Integer -> KVar
intKvar = KV . intSymbol "k_"

instance Show KVar where
  show (KV x) = "$" ++ show x

instance Hashable KVar
instance Hashable Brel
instance Hashable Bop
instance Hashable Var
instance Hashable VInfo
instance Hashable Constant

--------------------------------------------------------------------------------
-- | Substitutions -------------------------------------------------------------
--------------------------------------------------------------------------------

type Subst  = SSubst SrcSpan Var 
type BSubst = SSubst SrcSpan LocSymbol

newtype SSubst t s = Su (M.HashMap s (SExpr t s))
                deriving (Eq, Data, Typeable, Generic)


instance Fixpoint (SSubst t s) => Show (SSubst t s) where
  show = showFix

instance (Fixpoint s, Fixpoint (SExpr t s), Ord s) => Fixpoint (SSubst t s) where
  toFix (Su m) = case hashMapToAscList m of
                   []  -> empty
                   xys -> hcat $ map (\(x,y) -> brackets $ toFix x <> text ":=" <> toFix y) xys

--------------------------------------------------------------------------------
-- | Expressions ---------------------------------------------------------------
--------------------------------------------------------------------------------

data Constant = I !Integer
              | R !Double
              | L !Text !Sort
              deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Brel = Eq | Ne | Gt | Ge | Lt | Le | Ueq | Une
            deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Bop  = Plus | Minus | Times | Div | Mod
            deriving (Eq, Ord, Show, Data, Typeable, Generic)
              -- NOTE: For "Mod" 2nd expr should be a constant or a var *)

data Var = Var { vname :: Symbol
               , vsort :: Sort
               , vloc  :: SrcSpan
               , vinfo :: VInfo 
               } deriving (Ord, Show, Data, Typeable, Generic)

data VInfo = VInfo { isSMT :: Bool } 
           | VNoInfo 
           deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeSMTVar :: Symbol -> Sort -> Var
makeSMTVar x s 
  = Var x s dummySpan (VInfo True)

makeVar :: Symbol -> Sort -> Var
makeVar x s = Var x s dummySpan (VInfo False)

makeVarWithLoc :: SrcSpan -> Symbol -> Sort -> Var 
makeVarWithLoc l x s = (makeVar x s) {vloc = l}

locSymbolVar :: LocSymbol -> Sort -> Var 
locSymbolVar x s = (makeVar (val x) s) {vloc = srcSpan $ loc x}


mapVarSymbol f v = v {vname = f $ vname v}
mapVarSort   f v = v {vsort = f $ vsort v}

-- untyped expressions 
type BExpr = SExpr SrcSpan LocSymbol
type BReft = SReft SrcSpan LocSymbol

-- typed expressions
type Expr  = SExpr SrcSpan Var
type Reft  = SReft SrcSpan Var

data SExpr t s = 
  -- typed lambda calculus 
    EVar !s
  | EApp !(SExpr t s) !(SExpr t s)
  | ECon !Constant

  -- type instantiation
  | ECst !(SExpr t s) !Sort

  -- location information 
  | ETick !t !(SExpr t s)
   
  -- refined variables 
  | PKVar  !KVar !(SSubst t s)

  -- SMT specific expressions 
  | ENeg   !(SExpr t s)
  | EBin   !Bop !(SExpr t s) !(SExpr t s)
  | EIte   !(SExpr t s) !(SExpr t s) !(SExpr t s)

  | PAnd   ![(SExpr t s)]
  | POr    ![(SExpr t s)]
  | PNot   !(SExpr t s)
  | PImp   !(SExpr t s) !(SExpr t s)
  | PIff   !(SExpr t s) !(SExpr t s)
  | PAtom  !Brel  !(SExpr t s) !(SExpr t s)
  | PAll   ![(Symbol, Sort)] !(SExpr t s)
  | PExist ![(Symbol, Sort)] !(SExpr t s)

  deriving (Eq, Show, Data, Typeable, Generic)


newtype SReft t s = Reft (s, SExpr t s)
                  deriving (Eq, Data, Typeable, Generic)


reftparam (Reft (x,_)) = x 
reftSort = vsort . reftparam

--- NV we do not need that anymore 
data SortedReft = RR { sr_sort :: !Sort, sr_reft :: !Reft }
                  deriving (Eq, Data, Typeable, Generic)


-- | Functions that convert to old expressions 

pattern PTrue  = PAnd []
pattern PTop   = PAnd []
pattern PFalse = POr  [] 
pattern EBot   = POr  [] 

splitArgs :: Expr -> [Expr]
splitArgs = go []
  where
    go acc (EApp e1 e2) = go (e1:acc) e2
    go acc (ETick _ e)  = go acc e 
    go acc e            = e:reverse acc 

mkEApp :: Var -> [Expr] -> Expr
mkEApp f es = foldl EApp (EVar f) es

bmkEApp :: LocSymbol -> [BExpr] -> BExpr
bmkEApp f es = foldl EApp (ETick (srcSpan $ loc f) $ EVar f) es


elit :: Located Symbol -> Sort -> Expr
elit l s = ECon $ L (symbolText $ val l) s

symConst :: Text -> Constant
symConst = (`L` strSort)

instance Fixpoint Constant where
  toFix (I i)   = toFix i
  toFix (R i)   = toFix i
  toFix (L s t) | t == strSort = toFix $ encodeSymConst s 
  toFix (L s t) = parens $ text "lit" <+> text "\"" <> toFix s <> text "\"" <+> toFix t

instance Fixpoint Var where
  toFix = toFix . vname

instance Eq Var where
  v1 == v2 = (vname v1) == (vname v2)

instance Symbolic Var where
  symbol = vname


---------------------------------------------------------------
-- | String Constants -----------------------------------------
---------------------------------------------------------------

-- | Replace all symbol-representations-of-string-literals with string-literal
--   Used to transform parsed output from fixpoint back into fq.

-- instance Symbolic SymConst where
--   symbol = encodeSymConst

encodeSymConst  :: Text -> Symbol
encodeSymConst = litSymbol . symbol

decodeSymConst :: Symbol -> Maybe Text 
decodeSymConst = fmap (symbolText) . unLitSymbol

instance Fixpoint KVar where
  toFix (KV k) = text "$" <> toFix k

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
  toFix Plus  = text "+"
  toFix Minus = text "-"
  toFix Times = text "*"
  toFix Div   = text "/"
  toFix Mod   = text "mod"

instance (Fixpoint t, Eq s, Eq t, Ord t) => Fixpoint (SExpr s t) where
  toFix (ECon c)       = toFix c
  toFix (EVar s)       = toFix s
  toFix (EApp e1 e2)   = toFix e1 <> parens (toFix e2)
  toFix (ENeg e)       = parens $ text "-"  <+> parens (toFix e)
  toFix (EBin o e1 e2) = parens $ toFix e1  <+> toFix o <+> toFix e2
  toFix (EIte p e1 e2) = parens $ text "if" <+> toFix p <+> text "then" <+> toFix e1 <+> text "else" <+> toFix e2
  toFix (ECst e so)    = parens $ toFix e   <+> text " : " <+> toFix so
  toFix (EBot)         = text "_|_"
  toFix PTop             = text "???"
  toFix PTrue            = text "true"
  toFix PFalse           = text "false"
  toFix (PNot p)         = parens $ text "~" <+> parens (toFix p)
  toFix (PImp p1 p2)     = parens $ toFix p1 <+> text "=>" <+> toFix p2
  toFix (PIff p1 p2)     = parens $ toFix p1 <+> text "<=>" <+> toFix p2
  toFix (PAnd ps)        = text "&&" <+> toFix ps
  toFix (POr  ps)        = text "||" <+> toFix ps
  toFix (PAtom r e1 e2)  = parens $ toFix e1 <+> toFix r <+> toFix e2
  toFix (PKVar k su)     = toFix k <> toFix su
  toFix (PAll xts p)     = text "forall" <+> toFix xts <+> text "." <+> toFix p
  toFix (PExist xts p)   = text "exists" <+> toFix xts <+> text "." <+> toFix p
  toFix (ETick _ e)      = toFix e 

  simplify (PAnd [])     = PTrue
  simplify (POr  [])     = PFalse
  simplify (PAnd [p])    = simplify p
  simplify (POr  [p])    = simplify p

  simplify (PAnd ps)
    | any isContraPred ps = PFalse
    | otherwise           = PAnd $ filter (not . isTautoPred) $ map simplify ps

  simplify (POr  ps)
    | any isTautoPred ps = PTrue
    | otherwise          = POr  $ filter (not . isContraPred) $ map simplify ps

  simplify p
    | isContraPred p     = PFalse
    | isTautoPred  p     = PTrue
    | otherwise          = p

isContraPred   :: (Eq t, Eq s) => SExpr s t -> Bool
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

isTautoPred   :: (Eq t, Eq s) => SExpr s t -> Bool
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

isEVar :: Expr -> Bool
isEVar (EVar _) = True
isEVar _        = False

isEq  :: Brel -> Bool
isEq r          = r == Eq || r == Ueq




instance PPrint Constant where
  pprint (L x s) 
    | s == strSort = doubleQuotes $ text $ T.unpack x 
  pprint l      
    = toFix l

instance PPrint Brel where
  pprint Eq = text "=="
  pprint Ne = text "/="
  pprint r  = toFix r

instance PPrint Bop where
  pprint  = toFix

instance PPrint Sort where
  pprint = toFix

instance PPrint Var where
  pprint = pprint . vname 

instance PPrint KVar where
  pprint (KV x) = text "$" <> pprint x

-- | Wrap the enclosed 'Doc' in parentheses only if the condition holds.
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

opPrec Mod   = 5
opPrec Plus  = 6
opPrec Minus = 6
opPrec Times = 7
opPrec Div   = 7

instance PPrint Expr where
  pprintPrec _ (ECon c)        = pprint c
  pprintPrec _ (EVar s)        = pprint s
  pprintPrec _ (EBot)          = text "_|_"
  pprintPrec z (ENeg e)        = parensIf (z > zn) $
                                   text "-" <> pprintPrec (zn+1) e
    where zn = 2

  pprintPrec z (EApp e1 e2)    = parensIf (z > za) $
                                   pprintPrec (za+1) e1 <+> pprintPrec (za+1) e2
    where za = 8
  pprintPrec z (EBin o e1 e2)  = parensIf (z > zo) $
                                   pprintPrec (zo+1) e1 <+>
                                   pprint o             <+>
                                   pprintPrec (zo+1) e2
    where zo = opPrec o
  pprintPrec z (EIte p e1 e2)  = parensIf (z > zi) $
                                   text "if"   <+> pprintPrec (zi+1) p  <+>
                                   text "then" <+> pprintPrec (zi+1) e1 <+>
                                   text "else" <+> pprintPrec (zi+1) e2
    where zi = 1
  pprintPrec _ (ECst e so)     = parens $ pprint e <+> text ":" <+> pprint so

  pprintPrec _ PTop            = text "???"
  pprintPrec _ PTrue           = trueD
  pprintPrec _ PFalse          = falseD
  pprintPrec z (PNot p)        = parensIf (z > zn) $
                                   text "not" <+> pprintPrec (zn+1) p
    where zn = 8
  pprintPrec z (PImp p1 p2)    = parensIf (z > zi) $
                                   (pprintPrec (zi+1) p1) <+>
                                   text "=>"              <+>
                                   (pprintPrec (zi+1) p2)
    where zi = 2
  pprintPrec z (PIff p1 p2)    = parensIf (z > zi) $
                                   (pprintPrec (zi+1) p1) <+>
                                   text "<=>"             <+>
                                   (pprintPrec (zi+1) p2)
    where zi = 2
  pprintPrec z (PAnd ps)       = parensIf (z > za) $
                                   pprintBin (za+1) trueD  andD ps
    where za = 3
  pprintPrec z (POr  ps)       = parensIf (z > zo) $
                                   pprintBin (zo+1) falseD orD  ps
    where zo = 3
  pprintPrec z (PAtom r e1 e2) = parensIf (z > za) $
                                   pprintPrec (za+1) e1 <+>
                                   pprint r             <+>
                                   pprintPrec (za+1) e2
    where za = 4
  pprintPrec _ (PAll xts p)    = text "forall" <+> toFix xts <+> text "." <+> pprint p
  pprintPrec _ (PExist xts p)  = text "exists" <+> toFix xts <+> text "." <+> pprint p
  pprintPrec _ p@(PKVar {})    = toFix p
  pprintPrec z (ETick _ e)     = pprintPrec z e 

trueD  = text "true"
falseD = text "false"
andD   = text " &&"
orD    = text " ||"

pprintBin _ b _ [] = b
pprintBin z _ o xs = intersperse o $ pprintPrec z <$> xs

pprintReft :: Reft -> Doc
pprintReft (Reft (_,ra)) = pprintBin z trueD andD flat
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

instance Expression Expr where
  expr = id

-- | The symbol may be an encoding of a SymConst.

instance Expression Var where
  expr s = maybe (EVar s) (ECon . symConst) (decodeSymConst $ vname s)


exprSym :: LocSymbol -> BExpr
exprSym s = maybe (EVar s) (ECon . symConst) (decodeSymConst $ val s)

instance Expression Text where
  expr = ECon . symConst 

instance Expression Integer where
  expr = ECon . I

instance Expression Int where
  expr = expr . toInteger

instance Predicate Var where
  prop = eProp

instance Predicate Expr where
  prop = id

instance Predicate Bool where
  prop True  = PTrue
  prop False = PFalse

instance Expression a => Expression (Located a) where
  expr   = expr . val

eVar ::  Symbolic a => a -> BExpr
eVar = EVar . dummyLoc . symbol

eProp :: Var -> Expr
eProp = mkProp . EVar

isSingletonExpr :: Var -> Expr -> Maybe Expr
isSingletonExpr v (PAtom r e1 e2)
  | EVar v == e1 && isEq r = Just e2
  | EVar v == e1 && isEq r = Just e1
isSingletonExpr _ _        = Nothing

pAnd, pOr     :: (Eq s, Eq t, Fixpoint t, Ord t) => [SExpr s t] -> SExpr s t
pAnd          = simplify . PAnd
pOr           = simplify . POr

pIte p1 p2 p3 = pAnd [p1 `PImp` p2, (PNot p1) `PImp` p3]

mkProp :: Expr -> Expr 
mkProp        = EApp (EVar propVar)

propVar = makeVar propConName (FAbs 0 $ FFunc(FVar 0) boolSort)

--------------------------------------------------------------------------------
-- | Predicates ----------------------------------------------------------------
--------------------------------------------------------------------------------

isSingletonReft :: Reft -> Maybe Expr
isSingletonReft (Reft (v, ra)) = firstMaybe (isSingletonExpr v) $ conjuncts ra

relReft :: (Expression a) => Brel -> Var -> a -> Reft
relReft r v e  = Reft (v, PAtom r (EVar v)  (expr e))

exprReft, notExprReft, uexprReft ::  (Expression a) => Var -> a -> Reft
exprReft      = relReft Eq
notExprReft   = relReft Ne
uexprReft     = relReft Ueq

propReft      ::  (Predicate a) => Var -> a -> Reft
propReft v p  = Reft (v, PIff (mkProp $ EVar v) (prop p))

predReft      :: (Predicate a) => Var -> a -> Reft
predReft v p    = Reft (v, prop p)

reft :: Var -> Expr -> Reft
reft v p = Reft (v, p)

mapPredReft :: (Expr -> Expr) -> Reft -> Reft
mapPredReft f (Reft (v, p)) = Reft (v, f p)

---------------------------------------------------------------
-- | Refinements ----------------------------------------------
---------------------------------------------------------------

isFunctionReft :: Reft -> Bool
isFunctionReft = isFunctionSort . reftSort

isNonTrivial :: Reftable r => r -> Bool
isNonTrivial = not . isTauto

reftPred :: Reft -> Expr
reftPred (Reft (_, p)) = p

reftBind :: Reft -> Var
reftBind (Reft (x, _)) = x

------------------------------------------------------------
-- | Generally Useful Refinements --------------------------
------------------------------------------------------------

{- Plain symbols cannot create refinements any more 
symbolReft    :: (Symbolic a) => a -> Reft
symbolReft    = exprReft . eVar

usymbolReft   :: (Symbolic a) => a -> Reft
usymbolReft   = uexprReft . eVar
-}

vv_ :: Symbol
vv_ = vv Nothing


trueReft, falseReft :: Var -> Reft
trueReft  v = Reft (v, PTrue)
falseReft v = Reft (v, PFalse)

flattenRefas :: [Expr] -> [Expr]
flattenRefas        = concatMap flatP
  where
    flatP (PAnd ps) = concatMap flatP ps
    flatP p         = [p]

conjuncts :: (Eq s, Eq t) => SExpr s t -> [SExpr s t]
conjuncts (PAnd ps) = concatMap conjuncts ps
conjuncts p
  | isTautoPred p   = []
  | otherwise       = [p]

-------------------------------------------------------------------------
-- | TODO: This doesn't seem to merit a TC ------------------------------
-------------------------------------------------------------------------

class Falseable a where
  isFalse :: a -> Bool

instance Falseable Expr where
  isFalse (POr  []) = True
  isFalse _         = False

instance Falseable Reft where
  isFalse (Reft (_, ra)) = isFalse ra

-------------------------------------------------------------------------
-- | Class Predicates for Valid Refinements -----------------------------
-------------------------------------------------------------------------

class Subable a where
  syms   :: a -> [Var]
  substa :: (Var -> Var) -> a -> a
  -- substa f  = substf (EVar . f)

  substf :: (Var -> Expr) -> a -> a
  subst  :: Subst -> a -> a
  subst1 :: a -> (Var, Expr) -> a
  subst1 y (x, e) = subst (Su $ M.fromList [(x,e)]) y

instance Subable a => Subable (Located a) where
  syms (Loc _ _ x)   = syms x
  substa f (Loc l l' x) = Loc l l' (substa f x)
  substf f (Loc l l' x) = Loc l l' (substf f x)
  subst su (Loc l l' x) = Loc l l' (subst su x)


class (SG.Semigroup r, Subable r) => Reftable r where
  isTauto :: r -> Bool
  ppTy    :: r -> Doc -> Doc

  top     :: r -> r

  bot     :: r -> r

  meet    :: r -> r -> r
  meet    = (SG.<>)

  toReft  :: r -> Reft 
  ofReft  :: Reft -> r
  params  :: r -> [Var]          -- ^ parameters for Reft, vv + others
