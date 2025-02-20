{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE DeriveDataTypeable         #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

-- | This module contains the types defining an SMTLIB2 interface.

module Language.Fixpoint.Types.Theories (

    -- * Serialized Representation
      Raw

    -- * Theory Symbol
    , TheorySymbol (..)
    , Sem (..)

    -- * Theory Sorts
    , SmtSort (..)
    , sortSmtSort
    , isIntSmtSort

    -- * Symbol Environments
    , SymEnv (..)
    , symEnv
    , symEnvSort
    , symEnvTheory
    , insertSymEnv
    , deleteSymEnv
    , insertsSymEnv
    , symbolAtName
    , symbolAtSmtName

    -- * Coercing sorts in environments
    , coerceEnv
    , coerceSortEnv
    ) where


import           Data.Generics             (Data)
import           Data.Typeable             (Typeable)
import           Data.Hashable
import           GHC.Generics              (Generic)
import           Control.DeepSeq
import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Environments

import           Text.PrettyPrint.HughesPJ.Compat
import qualified Data.List                as L
import           Data.Text (Text)
import qualified Data.Text                as Text
import qualified Data.Store              as S
import qualified Data.HashMap.Strict      as M
import qualified Language.Fixpoint.Misc   as Misc

--------------------------------------------------------------------------------
-- | 'Raw' is the low-level representation for SMT values
--------------------------------------------------------------------------------
type Raw = Text

--------------------------------------------------------------------------------
-- | 'SymEnv' is used to resolve the 'Sort' and 'Sem' of each 'Symbol'
--------------------------------------------------------------------------------
data SymEnv = SymEnv
  { seSort   :: !(SEnv Sort)              -- ^ Sorts of *all* defined symbols
  , seTheory :: !(SEnv TheorySymbol)      -- ^ Information about theory-specific Symbols
  , seData   :: !(SEnv DataDecl)          -- ^ User-defined data-declarations
  , seLits   :: !(SEnv Sort)              -- ^ Distinct Constant symbols
  , seAppls  :: !(M.HashMap FuncSort Int) -- ^ Types at which `apply` was used;
                                           --   see [NOTE:apply-monomorphization]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

{- type FuncSort = {v:Sort | isFFunc v} @-}
type FuncSort = (SmtSort, SmtSort)

instance NFData   SymEnv
instance S.Store SymEnv

instance Semigroup SymEnv where
  e1 <> e2 = SymEnv { seSort   = seSort   e1 <> seSort   e2
                    , seTheory = seTheory e1 <> seTheory e2
                    , seData   = seData   e1 <> seData   e2
                    , seLits   = seLits   e1 <> seLits   e2
                    , seAppls  = seAppls  e1 <> seAppls  e2
                    }

instance Monoid SymEnv where
  mempty        = SymEnv emptySEnv emptySEnv emptySEnv emptySEnv mempty
  mappend       = (<>)

symEnv :: SEnv Sort -> SEnv TheorySymbol -> [DataDecl] -> SEnv Sort -> [Sort] -> SymEnv
symEnv xEnv fEnv ds ls ts = SymEnv xEnv' fEnv dEnv ls sortMap
  where
    xEnv'   = unionSEnv xEnv wiredInEnv
    dEnv    = fromListSEnv [(symbol d, d) | d <- ds]
    sortMap = M.fromList (zip smts [0..])
    smts    = funcSorts dEnv ts

-- | These are "BUILT-in" polymorphic functions which are
--   UNINTERPRETED but POLYMORPHIC, hence need to go through
--   the apply-defunc stuff.
wiredInEnv :: M.HashMap Symbol Sort
wiredInEnv = M.fromList
  [ (toIntName, mkFFunc 1 [FVar 0, FInt])
  , (tyCastName, FAbs 0 $ FAbs 1 $ FFunc (FVar 0) (FVar 1))
  ]


-- | 'funcSorts' attempts to compute a list of all the input-output sorts
--   at which applications occur. This is a gross hack; as during unfolding
--   we may create _new_ terms with weird new sorts. Ideally, we MUST allow
--   for EXTENDING the apply-sorts with those newly created terms.
--   the solution is perhaps to *preface* each VC query of the form
--
--      push
--      assert p
--      check-sat
--      pop
--
--   with the declarations needed to make 'p' well-sorted under SMT, i.e.
--   change the above to
--
--      declare apply-sorts
--      push
--      assert p
--      check-sat
--      pop
--
--   such a strategy would NUKE the entire apply-sort machinery from the CODE base.
--   [TODO]: dynamic-apply-declaration

funcSorts :: SEnv DataDecl -> [Sort] -> [FuncSort]
funcSorts dEnv ts = [ (t1, t2) | t1 <- smts, t2 <- smts]
  where
    smts = Misc.sortNub $ concat $ [ tx t1 ++ tx t2 | FFunc t1 t2 <- ts ]
    tx   = inlineArrSetBag False dEnv

-- Related to the above, after merging #688, we now allow types other than
-- Int to which Arrays/Sets/Bags can be applied.
-- However, the `sortSmtSort` function below, previously used in `funcSorts`,
-- only instantiates type variables at Ints. This causes the solver to crash
-- when PLE generates apply queries for polymorphic sets (see
-- https://github.com/ucsd-progsys/liquidhaskell/issues/2438). The following
-- pair of functions is a temporary fix for this - it generates additional
-- array/set/bag sorts instantiated at all user types for a "polymorphic depth 1"
-- (i.e., `Array (Foo Int) Int` but not `Array (Foo (Foo Int)) Int`, to keep
-- the applys table from blowing up exponentially). Ultimately, a general
-- solution should be implemented for generating ad-hoc sets of applys on the
-- fly, as described above.

inlineArrSetBag :: Bool -> SEnv DataDecl -> Sort -> [SmtSort]
inlineArrSetBag isASB env t = go . unAbs $ t
  where
    m = sortAbs t
    go (FFunc _ _)    = [SInt]
    go FInt           = [SInt]
    go FReal          = [SReal]
    go t
      | t == boolSort = [SBool]
      | isString t    = [SString]
    go (FVar _)
      | isASB     = SInt : map (\q -> let dd = snd q in
                                      SData (ddTyCon dd) (replicate (ddVars dd) SInt))
                               (M.toList $ seBinds env)
      | otherwise = [SInt]
    go t
      | (ct:ts) <- unFApp t = inlineArrSetBagFApp m env ct ts
      | otherwise = error "Unexpected empty 'unFApp t'"

inlineArrSetBagFApp :: Int -> SEnv DataDecl -> Sort -> [Sort] -> [SmtSort]
inlineArrSetBagFApp m env = go
  where
    go (FTC c) [a]
      | setConName == symbol c   = SSet <$> inlineArrSetBag True env a
    go (FTC c) [a]
      | bagConName == symbol c   = SBag <$> inlineArrSetBag True env a
    go (FTC c) [a, b]
      | arrayConName == symbol c = SArray <$> inlineArrSetBag True env a <*> inlineArrSetBag True env b
    go (FTC bv) [FTC s]
      | bitVecName == symbol bv
      , Just n <- sizeBv s      = [SBitVec n]
    go s []
      | isString s              = [SString]
    go (FTC c) ts
      | Just n <- tyArgs c env
      , let i = n - length ts   = [SData c ((inlineArrSetBag False env . FAbs m =<< ts) ++ replicate i SInt)]
    go _ _                      = [SInt]


symEnvTheory :: Symbol -> SymEnv -> Maybe TheorySymbol
symEnvTheory x env = lookupSEnv x (seTheory env)

symEnvSort :: Symbol -> SymEnv -> Maybe Sort
symEnvSort   x env = lookupSEnv x (seSort env)

insertSymEnv :: Symbol -> Sort -> SymEnv -> SymEnv
insertSymEnv x t env = env { seSort = insertSEnv x t (seSort env) }

deleteSymEnv :: Symbol -> SymEnv -> SymEnv
deleteSymEnv x env = env { seSort = deleteSEnv x (seSort env) }

insertsSymEnv :: SymEnv -> [(Symbol, Sort)] -> SymEnv
insertsSymEnv = L.foldl' (\env (x, s) -> insertSymEnv x s env)

symbolAtName :: (PPrint a) => Symbol -> SymEnv -> a -> Sort -> Text
symbolAtName mkSym env e = symbolAtSmtName mkSym env e . ffuncSort env
{-# SCC symbolAtName #-}

symbolAtSmtName :: (PPrint a) => Symbol -> SymEnv -> a -> FuncSort -> Text
symbolAtSmtName mkSym env e =
  -- formerly: intSymbol mkSym . funcSortIndex env e
  appendSymbolText mkSym . Text.pack . show . funcSortIndex env e
{-# SCC symbolAtSmtName #-}

funcSortIndex :: (PPrint a) => SymEnv -> a -> FuncSort -> Int
funcSortIndex env e fs = M.lookupDefault err fs (seAppls env)
  where
    err = panic ("Unknown func-sort: " ++ show fs ++ " for " ++ showpp e)

ffuncSort :: SymEnv -> Sort -> FuncSort
ffuncSort env t      = {- tracepp ("ffuncSort " ++ showpp (t1,t2)) -} (tx t1, tx t2)
  where
    tx               = applySmtSort (seData env)
    (t1, t2)         = args t
    args (FFunc a b) = (a, b)
    args _           = (FInt, FInt)

applySmtSort :: SEnv DataDecl -> Sort -> SmtSort
applySmtSort = sortSmtSort False

isIntSmtSort :: SEnv DataDecl -> Sort -> Bool
isIntSmtSort env s = SInt == applySmtSort env s

--------------------------------------------------------------------------------
-- | 'TheorySymbol' represents the information about each interpreted 'Symbol'
--------------------------------------------------------------------------------
data TheorySymbol  = Thy
  { tsSym    :: !Symbol          -- ^ name
  , tsRaw    :: !Raw             -- ^ serialized SMTLIB2 name
  , tsSort   :: !Sort            -- ^ sort
  , tsInterp :: !Sem             -- ^ TRUE = defined (interpreted), FALSE = declared (uninterpreted)
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Sem
instance NFData TheorySymbol
instance S.Store TheorySymbol

instance PPrint Sem where
  pprintTidy _ = text . show

instance Fixpoint TheorySymbol where
  toFix (Thy x _ t d) = text "TheorySymbol" <+> pprint (x, t) <+> parens (pprint d)

instance PPrint TheorySymbol where
  pprintTidy k (Thy x _ t d) = text "TheorySymbol" <+> pprintTidy k (x, t) <+> parens (pprint d)

--------------------------------------------------------------------------------
-- | 'Sem' describes the SMT semantics for a given symbol
--------------------------------------------------------------------------------

data Sem
  = Uninterp      -- ^ for UDF: `len`, `height`, `append`
  | Ctor          -- ^ for ADT constructor and tests: `cons`, `nil`
  | Test          -- ^ for ADT tests : `is$cons`
  | Field         -- ^ for ADT field: `hd`, `tl`
  | Theory        -- ^ for theory ops: mem, cup, select
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance S.Store Sem


--------------------------------------------------------------------------------
-- | A Refinement of 'Sort' that describes SMTLIB Sorts
--------------------------------------------------------------------------------
data SmtSort
  = SInt
  | SBool
  | SReal
  | SString
  | SSet !SmtSort
  | SBag !SmtSort
  | SArray !SmtSort !SmtSort
  | SBitVec !Int
  | SVar    !Int
  | SData   !FTycon ![SmtSort]
  -- HKT | SApp            ![SmtSort]           -- ^ Representing HKT
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SmtSort
instance NFData   SmtSort
instance S.Store SmtSort

-- | The 'poly' parameter is True when we are *declaring* sorts,
--   and so we need to leave the top type variables be; it is False when
--   we are declaring variables etc., and there, we serialize them
--   using `Int` (though really, there SHOULD BE NO floating tyVars...
--   'smtSort True  msg t' serializes a sort 't' using type variables,
--   'smtSort False msg t' serializes a sort 't' using 'Int' instead of tyvars.
sortSmtSort :: Bool -> SEnv DataDecl -> Sort -> SmtSort
sortSmtSort poly env t = {- tracepp ("sortSmtSort: " ++ showpp t) $ -} go . unAbs $ t
  where
    m = sortAbs t
    go (FFunc _ _)    = SInt
    go FInt           = SInt
    go FReal          = SReal
    go t
      | t == boolSort = SBool
      | isString t    = SString
    go (FVar i)
      | poly, i < m   = SVar i
      | otherwise     = SInt
    go t
      | (ct:ts) <- unFApp t = fappSmtSort poly m env ct ts
      | otherwise = error "Unexpected empty 'unFApp t'"

fappSmtSort :: Bool -> Int -> SEnv DataDecl -> Sort -> [Sort] -> SmtSort
fappSmtSort poly m env = go
  where
-- HKT    go t@(FVar _) ts            = SApp (sortSmtSort poly env <$> (t:ts))

    go (FTC c) [a]
      | setConName == symbol c  = SSet (sortSmtSort poly env a)
    go (FTC c) [a]
      | bagConName == symbol c  = SBag (sortSmtSort poly env a)
    go (FTC c) [a, b]
      | arrayConName == symbol c = SArray (sortSmtSort poly env a) (sortSmtSort poly env b)
    go (FTC bv) [FTC s]
      | bitVecName == symbol bv
      , Just n <- sizeBv s      = SBitVec n
    go s []
      | isString s              = SString
    go (FTC c) ts
      | Just n <- tyArgs c env
      , let i = n - length ts   = SData c ((sortSmtSort poly env . FAbs m <$> ts) ++ pad i)
    go _ _                      = SInt

    pad i | poly                = []
          | otherwise           = replicate i SInt

tyArgs :: (Symbolic x) => x -> SEnv DataDecl -> Maybe Int
tyArgs x env = ddVars <$> lookupSEnv (symbol x) env

instance PPrint SmtSort where
  pprintTidy _ SInt         = text "Int"
  pprintTidy _ SBool        = text "Bool"
  pprintTidy _ SReal        = text "Real"
  pprintTidy _ SString      = text "Str"
  pprintTidy k (SSet a)     = ppParens k (text "Set") [a]
  pprintTidy k (SBag a)     = ppParens k (text "Bag") [a]
  pprintTidy k (SArray a b) = ppParens k (text "Array") [a, b]
  pprintTidy _ (SBitVec n)  = text "BitVec" <+> int n
  pprintTidy _ (SVar i)     = text "@" <-> int i
--  HKT pprintTidy k (SApp ts)    = ppParens k (pprintTidy k tyAppName) ts
  pprintTidy k (SData c ts) = ppParens k (pprintTidy k c)         ts

ppParens :: (PPrint d) => Tidy -> Doc -> [d] -> Doc
ppParens k d ds = parens $ Misc.intersperse (text "") (d : (pprintTidy k <$> ds))

--------------------------------------------------------------------------------
-- | Coercing sorts inside environments for SMT theory encoding
--------------------------------------------------------------------------------

coerceSortEnv :: ElabFlags -> SEnv Sort -> SEnv Sort
coerceSortEnv ef ss = (if elabSetBag ef then coerceSetBagToArray else id) . coerceMapToArray <$> ss

coerceEnv :: ElabFlags -> SymEnv -> SymEnv
coerceEnv slv env =
  SymEnv { seSort   = coerceSortEnv slv (seSort env)
         , seTheory = seTheory env
         , seData   = seData   env
         , seLits   = seLits   env
         , seAppls  = seAppls  env
         }