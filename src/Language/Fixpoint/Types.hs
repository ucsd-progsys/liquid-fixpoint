{-# LANGUAGE NoMonomorphismRestriction, DeriveGeneric, DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}

-- | This module contains the data types, operations and serialization functions 
-- for representing Fixpoint's implication (i.e. subtyping) and well-formedness 
-- constraints in Haskell. The actual constraint solving is done by the
-- `fixpoint.native` which is written in Ocaml.

module Language.Fixpoint.Types (

  -- * Top level serialization  
    Fixpoint (..)
  , toFixpoint
  , FInfo (..)

  -- * Rendering
  , showFix
  , traceFix

  -- * Embedding to Fixpoint Types
  , Sort (..), FTycon, TCEmb
  , intFTyCon, boolFTyCon, propFTyCon, stringFTycon

  -- * Symbols
  , Symbol(..)
  , anfPrefix, tempPrefix, vv, intKvar
  , symChars, isNonSymbol, nonSymbol
  , isNontrivialVV
  , stringSymbol, symbolString
  
  -- * Creating Symbols
  , dummySymbol, intSymbol, tempSymbol
  , qualifySymbol, stringSymbolRaw
  , suffixSymbol

  -- * Expressions and Predicates
  , Constant (..), Bop (..), Brel (..), Expr (..), Pred (..)
  , eVar
  , eProp
  , pAnd, pOr, pIte, pApp
  , isTautoPred

  -- * Generalizing Embedding with Typeclasses 
  , Symbolic (..)
  , Expression (..)
  , Predicate (..)

  -- * Constraints and Solutions
  , SubC, WfC, subC, lhsCs, rhsCs, wfC, Tag, FixResult (..), FixSolution, addIds, sinfo 
  , trueSubCKvar
  , removeLhsKvars

  -- * Environments
  , SEnv, emptySEnv, fromListSEnv, mapSEnv, insertSEnv, deleteSEnv, memberSEnv, lookupSEnv
  , FEnv, insertFEnv 
  , IBindEnv, BindId, insertsIBindEnv, deleteIBindEnv, emptyIBindEnv
  , BindEnv, insertBindEnv, emptyBindEnv

  -- * Refinements
  , Refa (..), SortedReft (..), Reft(..), Reftable(..) 
 
  -- * Constructing Refinements
  , trueSortedReft          -- trivial reft
  , trueRefa                -- trivial reft
  , exprReft                -- singleton: v == e
  , notExprReft             -- singleton: v /= e
  , symbolReft              -- singleton: v == x
  , propReft                -- singleton: Prop(v) <=> p

  , isFunctionSortedReft
  , isNonTrivialSortedReft
  , isTautoReft
  , isSingletonReft
  , isEVar
  , isFalse
  , flattenRefas, shiftVV

  -- * Substitutions 
  , Subst, Subable (..)
  , emptySubst, mkSubst, catSubst
  , substExcept, substfExcept, subst1Except

  -- * Visitors
  , reftKVars

  -- * Functions on @Result@
  , colorResult 

  -- * Cut KVars
  , Kuts (..), ksEmpty, ksUnion

  -- * Qualifiers
  , Qualifier (..)
  ) where

import GHC.Generics         (Generic)
import Debug.Trace          (trace)

import Data.Monoid hiding   ((<>))
import Data.Functor
import Data.Char            (ord, chr, isAlpha, isUpper, toLower)
import Data.List            (sort)
import Data.Hashable        

import Data.Maybe           (fromMaybe)
import Text.Printf          (printf)
import Control.DeepSeq
import Control.Arrow        ((***))

import Language.Fixpoint.Misc
import Text.PrettyPrint.HughesPJ

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Language.Fixpoint.Names

class Fixpoint a where
  toFix    :: a -> Doc

  simplify :: a -> a 
  simplify =  id


showFix :: (Fixpoint a) => a -> String
showFix =  render . toFix

traceFix     ::  (Fixpoint a) => String -> a -> a
traceFix s x = trace ("\nTrace: [" ++ s ++ "] : " ++ showFix x) $ x


type TCEmb a    = M.HashMap a FTycon  

exprSymbols :: Expr -> [Symbol]
exprSymbols = go
  where 
    go (EVar x)        = [x]
    -- go (EDat x _)      = [x]
    go (ELit x _)      = [x]
    go (EApp f es)     = f : concatMap go es
    go (EBin _ e1 e2)  = go e1 ++ go e2 
    go (EIte p e1 e2)  = predSymbols p ++ go e1 ++ go e2 
    go (ECst e _)      = go e
    go _               = []

predSymbols :: Pred -> [Symbol]
predSymbols = go
  where 
    go (PAnd ps)        = concatMap go ps
    go (POr ps)         = concatMap go ps
    go (PNot p)         = go p
    go (PIff p1 p2)     = go p1 ++ go p2
    go (PImp p1 p2)     = go p1 ++ go p2
    go (PBexp e)        = exprSymbols e
    go (PAtom _ e1 e2)  = exprSymbols e1 ++ exprSymbols e2
    go (PAll xts p)     = (fst <$> xts) ++ go p
    go _                = []

reftKVars :: Reft -> [Symbol]
reftKVars (Reft (_,ras)) = [k | (RKvar k _) <- ras]

---------------------------------------------------------------
---------- (Kut) Sets of Kvars --------------------------------
---------------------------------------------------------------

newtype Kuts = KS (S.HashSet Symbol) 

instance NFData Kuts where
  rnf (KS _) = () -- rnf s

instance Fixpoint Kuts where
  toFix (KS s) = vcat $ ((text "cut " <>) . toFix) <$> S.toList s

ksEmpty             = KS S.empty
ksUnion kvs (KS s') = KS (S.union (S.fromList kvs) s')

---------------------------------------------------------------
---------- Converting Constraints to Fixpoint Input -----------
---------------------------------------------------------------

instance (Eq a, Hashable a, Fixpoint a) => Fixpoint (S.HashSet a) where
  toFix xs = brackets $ sep $ punctuate (text ";") (toFix <$> S.toList xs)
  simplify = S.fromList . map simplify . S.toList

instance Fixpoint a => Fixpoint (Maybe a) where
  toFix    = maybe (text "Nothing") ((text "Just" <+>) . toFix)
  simplify = fmap simplify

instance Fixpoint a => Fixpoint [a] where
  toFix xs = brackets $ sep $ punctuate (text ";") (fmap toFix xs)
  simplify = map simplify

instance (Fixpoint a, Fixpoint b) => Fixpoint (a,b) where
  toFix   (x,y)  = (toFix x) <+> text ":" <+> (toFix y)
  simplify (x,y) = (simplify x, simplify y) 

toFix_gs (SE e)        
  = vcat  $ map (toFix_constant . mapSnd sr_sort) $ hashMapToAscList e
toFix_constant (c, so) 
  = text "constant" <+> toFix c <+> text ":" <+> toFix so 



----------------------------------------------------------------------
------------------------ Type Constructors ---------------------------
----------------------------------------------------------------------

newtype FTycon = TC Symbol deriving (Eq, Ord, Show) -- Data, Typeable, Show)

intFTyCon  = TC (S "int")
boolFTyCon = TC (S "bool")
-- predFTyCon = TC (S "Pred")
propFTyCon = TC (S propConName)



-- listFTyCon = TC (S listConName)

-- isListTC   = (listFTyCon ==)
isListTC (TC (S c)) = c == listConName

stringFTycon :: String -> FTycon
stringFTycon c 
  | c == listConName = TC . S $ listConName
  | otherwise        = TC . stringSymbol $ dropModuleNames c


----------------------------------------------------------------------
------------------------------- Sorts --------------------------------
----------------------------------------------------------------------

data Sort = FInt 
          | FNum                 -- ^ numeric kind for Num tyvars
          | FObj  Symbol         -- ^ uninterpreted type
          | FVar  !Int           -- ^ fixpoint type variable
          | FFunc !Int ![Sort]   -- ^ type-var arity, in-ts ++ [out-t]
          | FApp FTycon [Sort]   -- ^ constructed type 
	      deriving (Eq, Ord, Show, Generic) --  Data, Typeable 

instance Hashable Sort

newtype Sub = Sub [(Int, Sort)]

instance Fixpoint Sort where
  toFix = toFix_sort

toFix_sort (FVar i)     = text "@"   <> parens (toFix i)
toFix_sort FInt         = text "int"
toFix_sort (FObj x)     = toFix x
toFix_sort FNum         = text "num"
toFix_sort (FFunc n ts) = text "func" <> parens ((toFix n) <> (text ", ") <> (toFix ts))
toFix_sort (FApp c [t]) 
  | isListTC c          = brackets $ toFix_sort t 
toFix_sort (FApp c ts)  = toFix c <+> intersperse space (fp <$> ts)
                          where fp s@(FApp _ (_:_)) = parens $ toFix_sort s 
                                fp s                = toFix_sort s


instance Fixpoint FTycon where
  toFix (TC s)       = toFix s


---------------------------------------------------------------
---------------------------- Symbols --------------------------
---------------------------------------------------------------

symChars 
  =  ['a' .. 'z']
  ++ ['A' .. 'Z'] 
  ++ ['0' .. '9'] 
  ++ ['_', '%', '.', '#']

data Symbol = S !String deriving (Eq, Ord) -- , Data, Typeable)

instance Fixpoint Symbol where
  toFix (S x) = text x

instance Show Symbol where
  show (S x) = x

instance Show Subst where
  show = showFix

instance Fixpoint Subst where
  toFix (Su m) = case {- hashMapToAscList -} m of 
                   []  -> empty
                   xys -> hcat $ map (\(x,y) -> brackets $ (toFix x) <> text ":=" <> (toFix y)) xys


---------------------------------------------------------------------------
------ Converting Strings To Fixpoint ------------------------------------- 
---------------------------------------------------------------------------

stringSymbolRaw :: String -> Symbol
stringSymbolRaw = S

stringSymbol :: String -> Symbol
stringSymbol s
  | isFixSym' s = S s 
  | otherwise   = S $ fixSymPrefix ++ concatMap encodeChar s

symbolString :: Symbol -> String
symbolString (S str) 
  = case chopPrefix fixSymPrefix str of
      Just s  -> concat $ zipWith tx indices $ chunks s 
      Nothing -> str
    where chunks = unIntersperse symSep 
          tx i s = if even i then s else [decodeStr s]

indices :: [Integer]
indices = [0..]

okSymChars
  =  ['a' .. 'z']
  ++ ['A' .. 'Z'] 
  ++ ['0' .. '9'] 
  ++ ['_', '.'  ]

symSep = '#'
fixSymPrefix = "fix" ++ [symSep]

suffixSymbol s suf = stringSymbol (symbolString s ++ suf)

isFixSym' (c:chs)  = isAlpha c && all (`elem` (symSep:okSymChars)) chs
isFixSym' _        = False

encodeChar c 
  | c `elem` okSymChars 
  = [c]
  | otherwise
  = [symSep] ++ (show $ ord c) ++ [symSep]

decodeStr s 
  = chr ((read s) :: Int)

qualifySymbol x sy 
  | isQualified x' = sy 
  | isParened x'   = stringSymbol (wrapParens (x ++ "." ++ stripParens x')) 
  | otherwise      = stringSymbol (x ++ "." ++ x')
  where x' = symbolString sy 

isQualified y         = '.' `elem` y 
wrapParens x          = "(" ++ x ++ ")"
isParened xs          = xs /= stripParens xs

---------------------------------------------------------------------

vv                  :: Maybe Integer -> Symbol
vv (Just i)         = S (vvName ++ [symSepName] ++ show i)
vv Nothing          = S vvName

vvCon               = S (vvName ++ [symSepName] ++ "F")

isNontrivialVV      = not . (vv_ ==) 


dummySymbol         = S dummyName
intSymbol x i       = S $ x ++ show i           

tempSymbol          ::  String -> Integer -> Symbol
tempSymbol prefix n = intSymbol (tempPrefix ++ prefix) n

tempPrefix          = "lq_tmp_"
anfPrefix           = "lq_anf_" 
nonSymbol           = S ""
isNonSymbol         = (0 ==) . length . symbolString

intKvar             :: Integer -> Symbol
intKvar             = intSymbol "k_" 

---------------------------------------------------------------
------------------------- Expressions -------------------------
---------------------------------------------------------------

data Constant = I !Integer 
              deriving (Eq, Ord, Show) 

data Brel = Eq | Ne | Gt | Ge | Lt | Le 
            deriving (Eq, Ord, Show) 

data Bop  = Plus | Minus | Times | Div | Mod    
            deriving (Eq, Ord, Show) 
	      -- NOTE: For "Mod" 2nd expr should be a constant or a var *)

data Expr = ECon !Constant 
          | EVar !Symbol
          | ELit !Symbol !Sort
          | EApp !Symbol ![Expr]
          | EBin !Bop !Expr !Expr
          | EIte !Pred !Expr !Expr
          | ECst !Expr !Sort
          | EBot
          deriving (Eq, Ord, Show)

instance Fixpoint Integer where
  toFix = integer 

instance Fixpoint Constant where
  toFix (I i) = toFix i


instance Fixpoint Brel where
  toFix Eq = text "="
  toFix Ne = text "!="
  toFix Gt = text ">"
  toFix Ge = text ">="
  toFix Lt = text "<"
  toFix Le = text "<="

instance Fixpoint Bop where
  toFix Plus  = text "+"
  toFix Minus = text "-"
  toFix Times = text "*"
  toFix Div   = text "/"
  toFix Mod   = text "mod"

instance Fixpoint Expr where
  toFix (ECon c)       = toFix c 
  toFix (EVar s)       = toFix s
  -- toFix (EDat s _)     = toFix s 
  toFix (ELit s _)     = toFix s
  toFix (EApp f es)    = (toFix f) <> (parens $ toFix es) 
  toFix (EBin o e1 e2) = parens $ toFix e1 <+> toFix o <+> toFix e2
  toFix (EIte p e1 e2) = parens $ toFix p <+> text "?" <+> toFix e1 <+> text ":" <+> toFix e2 
  toFix (ECst e so)    = parens $ toFix e <+> text " : " <+> toFix so 
  toFix (EBot)         = text "_|_"

----------------------------------------------------------
--------------------- Predicates -------------------------
----------------------------------------------------------

data Pred = PTrue
          | PFalse
          | PAnd  ![Pred]
          | POr   ![Pred]
          | PNot  !Pred
          | PImp  !Pred !Pred
          | PIff  !Pred !Pred
          | PBexp !Expr
          | PAtom !Brel !Expr !Expr
          | PAll  ![(Symbol, Sort)] !Pred
          | PTop
          deriving (Eq, Ord, Show) -- show Data, Typeable, Show)

instance Fixpoint Pred where
  toFix PTop             = text "???"
  toFix PTrue            = text "true"
  toFix PFalse           = text "false"
  toFix (PBexp e)        = parens $ text "?" <+> toFix e
  toFix (PNot p)         = parens $ text "~" <+> parens (toFix p)
  toFix (PImp p1 p2)     = parens $ (toFix p1) <+> text "=>" <+> (toFix p2)
  toFix (PIff p1 p2)     = parens $ (toFix p1) <+> text "<=>" <+> (toFix p2)
  toFix (PAnd ps)        = text "&&" <+> toFix ps
  toFix (POr  ps)        = text "||" <+> toFix ps
  toFix (PAtom r e1 e2)  = parens $ toFix e1 <+> toFix r <+> toFix e2
  toFix (PAll xts p)     = text "forall" <+> (toFix xts) <+> text "." <+> (toFix p)

  simplify (PAnd [])     = PTrue
  simplify (POr  [])     = PFalse
  simplify (PAnd [p])    = simplify p
  simplify (POr  [p])    = simplify p
  simplify (PAnd ps)    
    | any isContra ps    = PFalse
    | otherwise          = PAnd $ map simplify ps
  simplify (POr  ps)    
    | any isTautoPred ps = PTrue
    | otherwise          = POr  $ map simplify ps 
  simplify p            
    | isContra p         = PFalse
    | isTautoPred  p     = PTrue
    | otherwise          = p

zero         = ECon (I 0)
one          = ECon (I 1)
isContra     = (`elem` [ PAtom Eq zero one, PAtom Eq one zero, PFalse])   
isTautoPred  = (`elem` [ PTrue ])

isTautoReft (Reft (_, ras)) = all isTautoRa ras
isTautoRa (RConc p)         = isTautoPred p
isTautoRa _                 = False

isEVar (EVar _) = True
isEVar _        = False

isSingletonReft (Reft (v, [RConc (PAtom Eq e1 e2)])) 
  | e1 == EVar v = Just e2
  | e2 == EVar v = Just e1
isSingletonReft _    = Nothing 

pAnd          = simplify . PAnd 
pOr           = simplify . POr 
pIte p1 p2 p3 = pAnd [p1 `PImp` p2, (PNot p1) `PImp` p3] 


mkProp        = PBexp . EApp (S propConName) . (: [])


pApp :: Symbol -> [Expr] -> Pred
pApp p es= PBexp $ EApp (S ("papp" ++ show (length es))) (EVar p:es)

ppr_reft (Reft (v, ras)) d 
  | all isTautoRa ras
  = d
  | otherwise
  = braces (toFix v <+> colon <+> d <+> text "|" <+> ppRas ras)

ppr_reft_pred (Reft (_, ras))
  | all isTautoRa ras
  = text "true"
  | otherwise
  = ppRas ras

ppRas = cat . punctuate comma . map toFix . flattenRefas

------------------------------------------------------------------------
-- | Generalizing Symbol, Expression, Predicate into Classes -----------
------------------------------------------------------------------------

-- | Values that can be viewed as Symbols

class Symbolic a where
  symbol :: a -> Symbol

-- | Values that can be viewed as Expressions

class Expression a where
  expr   :: a -> Expr

-- | Values that can be viewed as Predicates

class Predicate a where
  prop   :: a -> Pred

instance Symbolic String where 
  symbol = stringSymbol

instance Symbolic Symbol where 
  symbol = id 

instance Expression Expr where
  expr = id

instance Expression Symbol where
  expr = eVar

instance Expression Integer where
  expr = ECon . I

instance Expression Int where
  expr = expr . toInteger

instance Predicate Symbol where
  prop = eProp

instance Predicate Pred where
  prop = id 


eVar          ::  Symbolic a => a -> Expr 
eVar          = EVar . symbol 

eProp         ::  Symbolic a => a -> Pred
eProp         = mkProp . eVar

exprReft, notExprReft  ::  (Expression a) => a -> Reft
exprReft e             = Reft (vv_, [RConc $ PAtom Eq (eVar vv_)  (expr e)])
notExprReft e          = Reft (vv_, [RConc $ PAtom Ne (eVar vv_)  (expr e)])

propReft               ::  (Predicate a) => a -> Reft
propReft p             = Reft (vv_, [RConc $ PIff     (eProp vv_) (prop p)]) 



---------------------------------------------------------------
----------------- Refinements ---------------------------------
---------------------------------------------------------------

data Refa 
  = RConc !Pred 
  | RKvar !Symbol !Subst
  deriving (Eq, Show)


newtype Reft = Reft (Symbol, [Refa]) deriving (Eq)

instance Show Reft where
  show (Reft x) = render $ toFix x 

data SortedReft = RR { sr_sort :: !Sort, sr_reft :: !Reft } deriving (Eq)

isNonTrivialSortedReft (RR _ (Reft (_, ras)))
  = not $ null ras

isFunctionSortedReft (RR (FFunc _ _) _)
  = True
isFunctionSortedReft _
  = False

sortedReftValueVariable (RR _ (Reft (v,_))) = v

---------------------------------------------------------------
----------------- Environments  -------------------------------
---------------------------------------------------------------

fromListSEnv            ::  [(Symbol, a)] -> SEnv a
fromListSEnv            = SE . M.fromList

mapSEnv f (SE env)      = SE (fmap f env)
deleteSEnv x (SE env)   = SE (M.delete x env)
insertSEnv x y (SE env) = SE (M.insert x y env)
lookupSEnv x (SE env)   = M.lookup x env
emptySEnv               = SE M.empty
memberSEnv x (SE env)   = M.member x env


-- | Functions for Indexed Bind Environment 

emptyIBindEnv :: IBindEnv
emptyIBindEnv = FB (S.empty)

deleteIBindEnv :: BindId -> IBindEnv -> IBindEnv
deleteIBindEnv i (FB s) = FB (S.delete i s)

insertsIBindEnv :: [BindId] -> IBindEnv -> IBindEnv
insertsIBindEnv is (FB s) = FB (foldr S.insert s is)

-- | Functions for Global Binder Environment
insertBindEnv :: Symbol -> SortedReft -> BindEnv -> (Int, BindEnv)
insertBindEnv x r (BE n m) = (n, BE (n + 1) (M.insert n (x, r) m))

emptyBindEnv :: BindEnv
emptyBindEnv = BE 0 M.empty


instance Functor SEnv where
  fmap f (SE m) = SE $ fmap f m

instance Fixpoint Refa where
  toFix (RConc p)    = toFix p
  toFix (RKvar k su) = toFix k <> toFix su
  -- toFix (RPvar p)    = toFix p

instance Fixpoint Reft where
  toFix = ppr_reft_pred

instance Fixpoint SortedReft where
  toFix (RR so (Reft (v, ras))) 
    = braces 
    $ (toFix v) <+> (text ":") <+> (toFix so) <+> (text "|") <+> toFix ras

instance Fixpoint FEnv where
  toFix (SE m)   = toFix (hashMapToAscList m)

instance Fixpoint BindEnv where
  toFix (BE _ m) = vcat $ map toFix_bind $ hashMapToAscList m 

toFix_bind (i, (x, r)) = text "bind" <+> toFix i <+> toFix x <+> text ":" <+> toFix r   

insertFEnv   = insertSEnv . lower 
  where lower x@(S (c:chs)) 
          | isUpper c = S $ toLower c : chs
          | otherwise = x
        lower z       = z

instance (Fixpoint a) => Fixpoint (SEnv a) where
  toFix (SE e) = vcat $ map pprxt $ hashMapToAscList e
	where pprxt (x, t) = toFix x <+> colon <> colon  <+> toFix t

instance Fixpoint (SEnv a) => Show (SEnv a) where
  show = render . toFix 

-----------------------------------------------------------------------------
------------------- Constraints ---------------------------------------------
-----------------------------------------------------------------------------

{-@ type Tag = { v : [Int] | len(v) = 1 } @-}
type Tag           = [Int] 

type BindId        = Int
type FEnv          = SEnv SortedReft 

newtype IBindEnv   = FB (S.HashSet BindId)
newtype SEnv a     = SE (M.HashMap Symbol a) deriving (Eq)
data BindEnv       = BE { be_size :: Int
                        , be_binds :: M.HashMap BindId (Symbol, SortedReft) 
                        }


data SubC a = SubC { senv  :: !IBindEnv
                   , sgrd  :: !Pred
                   , slhs  :: !SortedReft
                   , srhs  :: !SortedReft
                   , sid   :: !(Maybe Integer)
                   , stag  :: !Tag
                   , sinfo :: !a
                   }

data WfC a  = WfC  { wenv  :: !IBindEnv
                   , wrft  :: !SortedReft
                   , wid   :: !(Maybe Integer) 
                   , winfo :: !a
                   } -- deriving (Eq)

data FixResult a = Crash [a] String | Safe | Unsafe ![a] | UnknownError

type FixSolution = M.HashMap Symbol Pred

instance Monoid (FixResult a) where
  mempty                          = Safe
  mappend Safe x                  = x
  mappend x Safe                  = x
  mappend _ c@(Crash _ _)         = c 
  mappend c@(Crash _ _) _         = c 
  mappend (Unsafe xs) (Unsafe ys) = Unsafe (xs ++ ys)
  mappend UnknownError _          = UnknownError
  mappend _ UnknownError          = UnknownError

instance Functor FixResult where 
  fmap f (Crash xs msg) = Crash (f <$> xs) msg
  fmap f (Unsafe xs)    = Unsafe (f <$> xs)
  fmap _ Safe           = Safe
  fmap _ UnknownError   = UnknownError 

instance (Ord a, Fixpoint a) => Fixpoint (FixResult (SubC a)) where
  toFix Safe           = text "Safe"
  toFix UnknownError   = text "Unknown Error!"
  toFix (Crash xs msg) = vcat $ [ text "Crash!" ] ++  ppr_sinfos "CRASH: " xs ++ [parens (text msg)] 
  toFix (Unsafe xs)    = vcat $ text "Unsafe:" : ppr_sinfos "WARNING: " xs

ppr_sinfos :: (Ord a, Fixpoint a) => String -> [SubC a] -> [Doc]
ppr_sinfos msg = map ((text msg <>) . toFix) . sort . fmap sinfo


colorResult (Safe)      = Happy 
colorResult (Unsafe _)  = Angry 
colorResult (_)         = Sad 


instance Show (SubC a) where
  show = showFix 

instance Fixpoint (IBindEnv) where
  toFix (FB ids) = text "env" <+> toFix ids 

instance Fixpoint (SubC a) where
  toFix c     = hang (text "\n\nconstraint:") 2 bd
     where bd =   -- text "env" <+> toFix (senv c) 
                  toFix (senv c)
              $+$ text "grd" <+> toFix (sgrd c) 
              $+$ text "lhs" <+> toFix (slhs c) 
              $+$ text "rhs" <+> toFix (srhs c)
              $+$ (pprId (sid c) <+> pprTag (stag c)) 

instance Fixpoint (WfC a) where 
  toFix w     = hang (text "\n\nwf:") 2 bd 
    where bd  =   -- text "env"  <+> toFix (wenv w)
                  toFix (wenv w)
              $+$ text "reft" <+> toFix (wrft w) 
              $+$ pprId (wid w)

pprId (Just i)  = text "id" <+> (text $ show i)
pprId _         = text ""

pprTag []       = text ""
pprTag is       = text "tag" <+> toFix is 

instance Fixpoint Int where
  toFix = text . show 

-------------------------------------------------------
------------------- Substitutions ---------------------
-------------------------------------------------------

class Subable a where
  syms   :: a -> [Symbol]
  substa :: (Symbol -> Symbol) -> a -> a
  -- substa f  = substf (EVar . f) 
  
  substf :: (Symbol -> Expr) -> a -> a
  subst  :: Subst -> a -> a
  subst1 :: a -> (Symbol, Expr) -> a
  -- subst1 y (x, e) = subst (Su $ M.singleton x e) y
  subst1 y (x, e) = subst (Su [(x,e)]) y

subst1Except :: (Subable a) => [Symbol] -> a -> (Symbol, Expr) -> a
subst1Except xs z su@(x, _) 
  | x `elem` xs = z
  | otherwise   = subst1 z su

substfExcept :: (Symbol -> Expr) -> [Symbol] -> (Symbol -> Expr)
substfExcept f xs y = if y `elem` xs then EVar y else f y

substExcept  :: Subst -> [Symbol] -> Subst
-- substExcept  (Su m) xs = Su (foldr M.delete m xs) 
substExcept  (Su xes) xs = Su $ filter (not . (`elem` xs) . fst) xes

instance Subable Symbol where
  substa f x               = f x
  substf f x               = subSymbol (Just (f x)) x
  subst su x               = subSymbol (Just $ appSubst su x) x -- subSymbol (M.lookup x s) x
  syms x                   = [x]

subSymbol (Just (EVar y)) _ = y
subSymbol Nothing         x = x
subSymbol a               b = errorstar (printf "Cannot substitute symbol %s with expression %s" (showFix b) (showFix a))

instance Subable Expr where
  syms                     = exprSymbols
  substa f                 = substf (EVar . f) 
  substf f (EApp s es)     = EApp (substf f s) $ map (substf f) es 
  substf f (EBin op e1 e2) = EBin op (substf f e1) (substf f e2)
  substf f (EIte p e1 e2)  = EIte (substf f p) (substf f e1) (substf f e2)
  substf f (ECst e so)     = ECst (substf f e) so
  substf f e@(EVar x)      = f x 
  substf _ e               = e
 
  subst su (EApp f es)     = EApp (subst su f) $ map (subst su) es 
  subst su (EBin op e1 e2) = EBin op (subst su e1) (subst su e2)
  subst su (EIte p e1 e2)  = EIte (subst su p) (subst su e1) (subst  su e2)
  subst su (ECst e so)     = ECst (subst su e) so
  subst su (EVar x)        = appSubst su x
  subst _ e                = e


instance Subable Pred where
  syms                     = predSymbols
  substa f                 = substf (EVar . f) 
  substf f (PAnd ps)       = PAnd $ map (substf f) ps
  substf f (POr  ps)       = POr  $ map (substf f) ps
  substf f (PNot p)        = PNot $ substf f p
  substf f (PImp p1 p2)    = PImp (substf f p1) (substf f p2)
  substf f (PIff p1 p2)    = PIff (substf f p1) (substf f p2)
  substf f (PBexp e)       = PBexp $ substf f e
  substf f (PAtom r e1 e2) = PAtom r (substf f e1) (substf f e2)
  substf _  (PAll _ _)     = errorstar $ "substf: FORALL" 
  substf _  p              = p

  subst su (PAnd ps)       = PAnd $ map (subst su) ps
  subst su (POr  ps)       = POr  $ map (subst su) ps
  subst su (PNot p)        = PNot $ subst su p
  subst su (PImp p1 p2)    = PImp (subst su p1) (subst su p2)
  subst su (PIff p1 p2)    = PIff (subst su p1) (subst su p2)
  subst su (PBexp e)       = PBexp $ subst su e
  subst su (PAtom r e1 e2) = PAtom r (subst su e1) (subst su e2)
  subst _  (PAll _ _)      = errorstar $ "subst: FORALL" 
  subst _  p               = p

instance Subable Refa where
  syms (RConc p)           = syms p
  syms (RKvar k (Su su'))  = k : concatMap syms ({- M.elems -} su') 
  subst su (RConc p)       = RConc   $ subst su p
  subst su (RKvar k su')   = RKvar k $ su' `catSubst` su 
  -- subst _  (RPvar p)     = RPvar p
  substa f                 = substf (EVar . f) 
  substf f (RConc p)       = RConc (substf f p)
  substf _ ra@(RKvar _ _)  = ra

instance (Subable a, Subable b) => Subable (a,b) where
  syms  (x, y)   = syms x ++ syms y
  subst su (x,y) = (subst su x, subst su y)
  substf f (x,y) = (substf f x, substf f y)
  substa f (x,y) = (substa f x, substa f y)

instance Subable a => Subable [a] where
  syms   = concatMap syms
  subst  = map . subst 
  substf = map . substf 
  substa = map . substa 

instance Subable a => Subable (M.HashMap k a) where
  syms   = syms . M.elems 
  subst  = M.map . subst 
  substf = M.map . substf 
  substa = M.map . substa

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


-- newtype Subst  = Su (M.HashMap Symbol Expr) deriving (Eq)
newtype Subst = Su [(Symbol, Expr)] deriving (Eq)

mkSubst                  = Su -- . M.fromList
appSubst (Su s) x        = fromMaybe (EVar x) (lookup x s)
emptySubst               = Su [] -- M.empty
catSubst (Su s1) (Su s2) = Su $ s1' ++ s2
  where s1' = mapSnd (subst (Su s2)) <$> s1
  -- = Su $ s1' `M.union` s2
  --   where s1' = subst (Su s2) `M.map` s1

------------------------------------------------------------
------------- Generally Useful Refinements -----------------
------------------------------------------------------------

symbolReft    = exprReft . eVar 

vv_           = vv Nothing



trueSortedReft :: Sort -> SortedReft
trueSortedReft = (`RR` trueReft) 

trueReft = Reft (vv_, [])

trueRefa = RConc PTrue

flattenRefas ::  [Refa] -> [Refa]
flattenRefas = concatMap flatRa
  where flatRa (RConc p) = RConc <$> flatP p
        flatRa ra        = [ra]
        flatP  (PAnd ps) = concatMap flatP ps
        flatP  p         = [p]

----------------------------------------------------------------
---------------------- Strictness ------------------------------
----------------------------------------------------------------

instance NFData Symbol where
  rnf (S x) = rnf x

instance NFData FTycon where
  rnf (TC c)       = rnf c

instance NFData Sort where
  rnf (FVar x)     = rnf x
  rnf (FFunc n ts) = rnf n `seq` (rnf <$> ts) `seq` () 
  rnf (FApp c ts)  = rnf c `seq` (rnf <$> ts) `seq` ()
  rnf (z)          = z `seq` ()

instance NFData Sub where
  rnf (Sub x) = rnf x

instance NFData Subst where
  rnf (Su x) = rnf x

instance NFData FEnv where
  rnf (SE x) = rnf x

instance NFData IBindEnv where
  rnf (FB x) = rnf x

instance NFData BindEnv where
  rnf (BE x m) = rnf x `seq` rnf m


instance NFData Constant where
  rnf (I x) = rnf x

instance NFData Brel 
instance NFData Bop

instance NFData Expr where
  rnf (ECon x)        = rnf x
  rnf (EVar x)        = rnf x
  -- rnf (EDat x1 x2)    = rnf x1 `seq` rnf x2
  rnf (ELit x1 x2)    = rnf x1 `seq` rnf x2
  rnf (EApp x1 x2)    = rnf x1 `seq` rnf x2
  rnf (EBin x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3
  rnf (EIte x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3
  rnf (ECst x1 x2)    = rnf x1 `seq` rnf x2
  rnf (_)             = ()

instance NFData Pred where
  rnf (PAnd x)         = rnf x
  rnf (POr  x)         = rnf x
  rnf (PNot x)         = rnf x
  rnf (PBexp x)        = rnf x
  rnf (PImp x1 x2)     = rnf x1 `seq` rnf x2
  rnf (PIff x1 x2)     = rnf x1 `seq` rnf x2
  rnf (PAll x1 x2)     = rnf x1 `seq` rnf x2
  rnf (PAtom x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3
  rnf (_)              = ()

instance NFData Refa where
  rnf (RConc x)     = rnf x
  rnf (RKvar x1 x2) = rnf x1 `seq` rnf x2
  -- rnf (RPvar _)     = () -- rnf x

instance NFData Reft where 
  rnf (Reft (v, ras)) = rnf v `seq` rnf ras

instance NFData SortedReft where 
  rnf (RR so r) = rnf so `seq` rnf r

instance (NFData a) => NFData (SubC a) where
  rnf (SubC x1 x2 x3 x4 x5 x6 x7) 
    = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7

instance (NFData a) => NFData (WfC a) where
  rnf (WfC x1 x2 x3 x4) 
    = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4

----------------------------------------------------------------------------
-------------- Hashable Instances -----------------------------------------
---------------------------------------------------------------------------

instance Hashable Symbol where 
  hashWithSalt i (S s) = hashWithSalt i s

instance Hashable FTycon where
  hashWithSalt i (TC s) = hashWithSalt i s

--------------------------------------------------------------------------------------
-------- Constraint Constructor Wrappers ---------------------------------------------
--------------------------------------------------------------------------------------

wfC  = WfC

-- subC γ p r1@(RR _ (Reft (v,_))) (RR t2 r2) x y z 
--    = SubC γ p r1 (RR t2 (shiftVV r2 v)) x y z
subC γ p (RR t1 r1) (RR t2 r2) x y z 
    = SubC γ p (RR t1 (shiftVV r1 vvCon)) (RR t2 (shiftVV r2 vvCon)) x y z

lhsCs = sr_reft . slhs
rhsCs = sr_reft . srhs

removeLhsKvars cs vs 
  = cs{slhs = goRR (slhs cs)} 
  where goRR rr                     = rr{sr_reft = goReft (sr_reft rr)} 
        goReft (Reft(v, rs))        = Reft(v, filter f rs)
        f (RKvar v _) | v `elem` vs = False
        f r                         = True 
        
trueSubCKvar v
  = subC emptyIBindEnv PTrue mempty (RR mempty (Reft(vv_, [RKvar v emptySubst]))) Nothing [0] 

shiftVV r@(Reft (v, ras)) v' 
   | v == v'   = r
   | otherwise = Reft (v', (subst1 ras (v, EVar v')))


addIds = zipWith (\i c -> (i, shiftId i $ c {sid = Just i})) [1..]
  where -- Adding shiftId to have distinct VV for SMT conversion 
    shiftId i c = c { slhs = shiftSR i $ slhs c } 
                    { srhs = shiftSR i $ srhs c }
    shiftSR i sr = sr { sr_reft = shiftR i $ sr_reft sr }
    shiftR i r@(Reft (S v, _)) = shiftVV r (S (v ++ show i))


-- subC γ p r1 r2 x y z   = (vvsu, SubC γ p r1' r2' x y z)
--   where (vvsu, r1', r2') = unifySRefts r1 r2 

-- unifySRefts (RR t1 r1) (RR t2 r2) = (z, RR t1 r1', RR t2 r2')
--   where (r1', r2')                =  unifyRefts r1 r2

-- unifyRefts r1@(Reft (v1, _)) r2@(Reft (v2, _))
--    | v1 == v2  = (r1, r2)
--    | otherwise = (r1, shiftVV r2 v1)

-- unifySRefts (RR t1 r1) (RR t2 r2) = (z, RR t1 r1', RR t2 r2')
--   where (z, r1', r2')             =  unifyRefts r1 r2
--
-- unifyRefts r1@(Reft (v1, _)) r2@(Reft (v2, _))
--   | v1 == v2  = ((v1, emptySubst), r1, r2)
--   | v1 /= vv_ = let (su, r2') = shiftVV r2 v1 in ((v1, su), r1 , r2')
--   | otherwise = let (su, r1') = shiftVV r1 v2 in ((v2, su), r1', r2 ) 
--
-- shiftVV (Reft (v, ras)) v' = (su, (Reft (v', subst su ras))) 
--   where su = mkSubst [(v, EVar v')]


------------------------------------------------------------------------
----------------- Qualifiers -------------------------------------------
------------------------------------------------------------------------


data Qualifier = Q { q_name   :: String           -- ^ Name
                   , q_params :: [(Symbol, Sort)] -- ^ Parameters
                   , q_body   :: Pred             -- ^ Predicate
                   }
               deriving (Eq, Ord)  

instance Fixpoint Qualifier where 
  toFix = pprQual

instance NFData Qualifier where
  rnf (Q x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3

pprQual (Q n xts p) = text "qualif" <+> text n <> parens args  <> colon <+> toFix p 
  where args = intersperse comma (toFix <$> xts)

data FInfo a = FI { cm    :: M.HashMap Integer (SubC a)
                  , ws    :: ![WfC a] 
                  , bs    :: !BindEnv
                  , gs    :: !FEnv
                  , lits  :: ![(Symbol, Sort)]
                  , kuts  :: Kuts 
                  , quals :: ![Qualifier]
                  }

-- toFixs = brackets . hsep . punctuate comma -- . map toFix 

toFixpoint x'    = kutsDoc x' $+$ gsDoc x' $+$ conDoc x' $+$ bindsDoc x' $+$ csDoc x' $+$ wsDoc x'
  where conDoc   = vcat     . map toFix_constant . lits
        csDoc    = vcat     . map toFix . M.elems . cm 
        wsDoc    = vcat     . map toFix . ws 
        kutsDoc  = toFix    . kuts
        bindsDoc = toFix    . bs
        gsDoc    = toFix_gs . gs


-------------------------------------------------------------------------
-- | A Class Predicates for Valid Refinements Types ---------------------
-------------------------------------------------------------------------

class (Monoid r, Subable r, Fixpoint r) => Reftable r where 
  isTauto :: r -> Bool
  ppTy    :: r -> Doc -> Doc
  
  top     :: r
  top     =  mempty
  
  meet    :: r -> r -> r
  meet    = mappend

  toReft  :: r -> Reft
  params  :: r -> [Symbol]          -- ^ parameters for Reft, vv + others

instance Monoid Reft where
  mempty  = trueReft
  mappend = meetReft

meetReft r@(Reft (v, ras)) r'@(Reft (v', ras')) 
  | v == v'          = Reft (v , ras  ++ ras')
  | v == dummySymbol = Reft (v', ras' ++ (ras `subst1`  (v , EVar v'))) 
  | otherwise        = Reft (v , ras  ++ (ras' `subst1` (v', EVar v )))

instance Reftable Reft where
  isTauto  = isTautoReft
  ppTy     = ppr_reft
  toReft   = id
  params _ = []

instance Monoid Sort where
  mempty            = FObj (S "any")
  mappend t1 t2 
    | t1 == mempty  = t2
    | t2 == mempty  = t1
    | t1 == t2      = t1
    | otherwise     = errorstar $ "mappend-sort: conflicting sorts t1 =" ++ show t1 ++ " t2 = " ++ show t2

instance Monoid SortedReft where
  mempty        = RR mempty mempty
  mappend t1 t2 = RR (mappend (sr_sort t1) (sr_sort t2)) (mappend (sr_reft t1) (sr_reft t2))

instance Reftable SortedReft where
  isTauto  = isTauto . sr_reft
  ppTy     = ppTy    . sr_reft
  toReft   = sr_reft
  params _ = []

class Falseable a where
  isFalse :: a -> Bool

instance Falseable Pred where
  isFalse (PFalse) = True
  isFalse _        = False

instance Falseable Refa where
  isFalse (RConc p) = isFalse p
  isFalse _         = False

instance Falseable Reft where
  isFalse (Reft(_, rs)) = or [isFalse p | RConc p <- rs]

