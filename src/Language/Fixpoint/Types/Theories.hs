{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE DeriveDataTypeable         #-}

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
    , insertsSymEnv
    , symbolAtName
    , symbolAtSmtName


    ) where


import           Data.Generics             (Data)
#if !MIN_VERSION_base(4,14,0)
import           Data.Semigroup            (Semigroup (..))
#endif

import           Data.Typeable             (Typeable)
import           Data.Hashable
import           GHC.Generics              (Generic)
import           Control.DeepSeq
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Environments

import           Text.PrettyPrint.HughesPJ.Compat
import qualified Data.List                as L
import           Data.Text (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Store              as S
import qualified Data.HashMap.Strict      as M
import qualified Language.Fixpoint.Misc   as Misc
import Data.Functor.Contravariant (Contravariant(contramap))

--------------------------------------------------------------------------------
-- | 'Raw' is the low-level representation for SMT values
--------------------------------------------------------------------------------
type Raw = LT.Text

instance S.Store Raw where
  peek = LT.fromStrict <$> S.peek
  poke = S.poke . LT.toStrict
  size = contramap LT.toStrict S.size

--------------------------------------------------------------------------------
-- | 'SymEnv' is used to resolve the 'Sort' and 'Sem' of each 'Symbol'
--------------------------------------------------------------------------------
data SymEnv = SymEnv
  { seSort    :: !(SEnv Sort)              -- ^ Sorts of *all* defined symbols
  , seTheory  :: !(SEnv TheorySymbol)      -- ^ Information about theory-specific Symbols
  , seData    :: !(SEnv DataDecl)          -- ^ User-defined data-declarations
  , seLits    :: !(SEnv Sort)              -- ^ Distinct Constant symbols
  , seAppls   :: !(M.HashMap FuncSort Int) -- ^ Types at which `apply` was used;
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
    xEnv'                 = unionSEnv xEnv wiredInEnv
    dEnv                  = fromListSEnv [(symbol d, d) | d <- ds]
    sortMap               = M.fromList (zip smts [0..])
    smts                  = funcSorts dEnv ts

-- | These are "BUILT-in" polymorphic functions which are
--   UNININTERPRETED but POLYMORPHIC, hence need to go through
--   the apply-defunc stuff.
wiredInEnv :: M.HashMap Symbol Sort
wiredInEnv = M.fromList
  [ (toIntName, mkFFunc 1 [FVar 0, FInt])
  , (tyCastName, FAbs 0 $ FAbs 1 $ FFunc (FVar 0) (FVar 1))
  ]


-- | 'smtSorts' attempts to compute a list of all the input-output sorts
--   at which applications occur. This is a gross hack; as during unfolding
--   we may create _new_ terms with wierd new sorts. Ideally, we MUST allow
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
    smts         = Misc.sortNub $ concat [ [tx t1, tx t2] | FFunc t1 t2 <- ts]
    tx           = applySmtSort dEnv


symEnvTheory :: Symbol -> SymEnv -> Maybe TheorySymbol
symEnvTheory x env = lookupSEnv x (seTheory env)

symEnvSort :: Symbol -> SymEnv -> Maybe Sort
symEnvSort   x env = lookupSEnv x (seSort env)

insertSymEnv :: Symbol -> Sort -> SymEnv -> SymEnv
insertSymEnv x t env = env { seSort = insertSEnv x t (seSort env) }

insertsSymEnv :: SymEnv -> [(Symbol, Sort)] -> SymEnv
insertsSymEnv = L.foldl' (\env (x, s) -> insertSymEnv x s env)

symbolAtName :: (PPrint a) => Symbol -> SymEnv -> a -> Sort -> Text
symbolAtName mkSym env e = symbolAtSmtName mkSym env e . ffuncSort env
{-# SCC symbolAtName #-}

symbolAtSmtName :: (PPrint a) => Symbol -> SymEnv -> a -> FuncSort -> Text
symbolAtSmtName mkSym env e s =
  -- formerly: intSymbol mkSym . funcSortIndex env e
  appendSymbolText mkSym $ Text.pack (show (funcSortIndex env e s))
{-# SCC symbolAtSmtName #-}

funcSortIndex :: (PPrint a) => SymEnv -> a -> FuncSort -> Int
funcSortIndex env e z = M.lookupDefault err z (seAppls env)
  where
    err               = panic ("Unknown func-sort: " ++ showpp z ++ " for " ++ showpp e)

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
  | Ctor         -- ^ for ADT constructor and tests: `cons`, `nil`
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
  | SSet
  | SMap
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
sortSmtSort poly env t  = {- tracepp ("sortSmtSort: " ++ showpp t) else id) $ -}  go . unAbs $ t
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
    go t              = fappSmtSort poly m env ct ts where (ct:ts) = unFApp t

fappSmtSort :: Bool -> Int -> SEnv DataDecl -> Sort -> [Sort] -> SmtSort
fappSmtSort poly m env = go
  where
-- HKT    go t@(FVar _) ts            = SApp (sortSmtSort poly env <$> (t:ts))
    go (FTC c) _
      | setConName == symbol c  = SSet
    go (FTC c) _
      | mapConName == symbol c  = SMap
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
  pprintTidy _ SSet         = text "Set"
  pprintTidy _ SMap         = text "Map"
  pprintTidy _ (SBitVec n)  = text "BitVec" <+> int n
  pprintTidy _ (SVar i)     = text "@" <-> int i
--  HKT pprintTidy k (SApp ts)    = ppParens k (pprintTidy k tyAppName) ts
  pprintTidy k (SData c ts) = ppParens k (pprintTidy k c)         ts

ppParens :: (PPrint d) => Tidy -> Doc -> [d] -> Doc
ppParens k d ds = parens $ Misc.intersperse (text "") (d : (pprintTidy k <$> ds))
