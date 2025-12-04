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
    , FuncSort
    , sortSmtSort
    , isIntSmtSort

    , mergeTopAppls
    , pushAppls
    , popAppls
    , peekAppls

    -- * Symbol Environments
    , SymEnv (..)
    , SymM
    , symEnv
    , symEnvSort
    , symEnvTheory
    , insertSymEnv
    , deleteSymEnv
    , insertsSymEnv
    , symbolAtName
    , symbolAtSortIndex

    -- * Coercing sorts in environments
    , coerceSort
    , coerceEnv
    , coerceSortEnv
    , TheorySymbols(..)
    ) where


import           Data.Generics             (Data)
import           Data.Typeable             (Typeable)
import           Data.Hashable
import           GHC.Generics              (Generic)
import           Control.Applicative
import           Control.Monad.State
import           Control.DeepSeq
import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Sorts
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

-- | Apply tags already used to declare @apply@ symbols in the SMT solver.
--
-- The tags are organized in a stack because every time we pop the SMT solver
-- state, it forgets the tags declared since the last push.
--
-- Each entry in the stack describes the integer tag corresponding to a
-- particular function sort. Every time we issue a `push` a new level
-- is added to the stack, and correspondingly, a `pop` removes a level.
--
-- See 'seApplsCur' in 'SymEnv' for details about actually declaring the
-- tags to the SMT solver.
type Appls = [M.HashMap FuncSort Int]

lookupAppls :: FuncSort -> Appls -> Maybe Int
lookupAppls fs = foldr (\hm acc -> acc <|> M.lookup fs hm) Nothing

mergeTopAppls :: M.HashMap FuncSort Int -> Appls -> Appls
mergeTopAppls m (top : rest) = (top <> m) : rest
mergeTopAppls m [] = [m]

pushAppls :: Appls -> Appls
pushAppls aps = M.empty : aps

popAppls :: Appls -> Appls
popAppls [] = []
popAppls (_:xs) = xs

peekAppls :: Appls -> Maybe (M.HashMap FuncSort Int)
peekAppls [] = Nothing
peekAppls (x:_) = Just x

data SymEnv = SymEnv
  { seSort     :: !(SEnv Sort)              -- ^ Sorts of *all* defined symbols
  , seTheory   :: !(SEnv TheorySymbol)      -- ^ Information about theory-specific Symbols
  , seData     :: !(SEnv DataDecl)          -- ^ User-defined data-declarations
  , seLits     :: !(SEnv Sort)              -- ^ Distinct Constant symbols

    -- | Apply tags already declared in the SMT solver.
    --
    -- This is inspected when serializing applications of functions to determine
    -- if a new tag needs to be created for a given function sort
    -- (@funcSortIndex@).
  , seAppls    :: !Appls

    -- | Apply tags that have been created while serializing expressions for the
    -- SMT solver, but which have not been used to declare apply symbols yet in
    -- the SMT solver.
    --
    -- The apply symbols using the tags are declared whenever we need to send
    -- the serialized expressions to the SMT solver (using @funcSortVars@). At
    -- this point, the contents of this map are merged into the top of the
    -- 'seAppls' stack, and @seApplsCur@ is cleared.
  , seApplsCur :: !(M.HashMap FuncSort Int)
  , seIx       :: !Int                      -- ^ Largest unused index for sorts
  }
  deriving (Eq, Show, Data, Typeable, Generic)

{- type FuncSort = {v:Sort | isFFunc v} @-}
type FuncSort = (SmtSort, SmtSort)

-- | Generating SMT expressions is a stateful process because new symbols ('apply', 'coerce',
--   'smt_lambda' and 'lam_arg') need to be emitted with unique ids for each newly encountered
--   function sort. The 'SymM' monad carries the 'SymEnv' state required to track the ids.
--   The state updates are performed in `L.F.Smt.Serialize` (functions `smt2App`, `smt2Coerc`,
--   `smt2Lam` and `smtLamArg`, correspondingly).
type SymM a = State SymEnv a

instance NFData   SymEnv
instance S.Store SymEnv

instance Semigroup SymEnv where
  e1 <> e2 = SymEnv { seSort     = seSort     e1 <> seSort     e2
                    , seTheory   = seTheory   e1 <> seTheory   e2
                    , seData     = seData     e1 <> seData     e2
                    , seLits     = seLits     e1 <> seLits     e2
                    , seAppls    = zipWith (<>) (seAppls e1) (seAppls e2)
                    , seApplsCur = seApplsCur e1 <> seApplsCur e2
                    , seIx       = seIx       e1 `max` seIx    e2
                    }

instance Monoid SymEnv where
  mempty        = SymEnv emptySEnv emptySEnv emptySEnv emptySEnv [] mempty 0
  mappend       = (<>)

symEnv :: SEnv Sort -> SEnv TheorySymbol -> [DataDecl] -> SEnv Sort -> [Sort] -> SymEnv
symEnv xEnv fEnv ds ls _ = SymEnv xEnv' fEnv dEnv ls [] mempty 0
  where
    xEnv'   = unionSEnv xEnv wiredInEnv
    dEnv    = fromListSEnv [(symbol d, d) | d <- ds]

-- | These are "BUILT-in" polymorphic functions which are
--   UNINTERPRETED but POLYMORPHIC, hence need to go through
--   the apply-defunc stuff.
wiredInEnv :: M.HashMap Symbol Sort
wiredInEnv = M.fromList
  [ (toIntName, mkFFunc 1 [FVar 0, FInt])
  , (tyCastName, FAbs 0 $ FAbs 1 $ FFunc (FVar 0) (FVar 1))
  ]

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

symbolAtSortIndex :: Symbol -> Int -> Text
symbolAtSortIndex mkSym si = appendSymbolText mkSym . Text.pack . show $ si

symbolAtName :: Symbol -> Sort -> SymM Text
symbolAtName mkSym s =
  do env <- get
     fsi <- funcSortIndex (ffuncSort env s)
     pure $ symbolAtSortIndex mkSym fsi
{-# SCC symbolAtName #-}

-- See 'seAppls' and 'seApplsCur' in 'SymEnv' for explanation.
funcSortIndex :: FuncSort -> SymM Int
funcSortIndex fs =
  do env <- get
     let aps = seAppls env
     let apsc = seApplsCur env
     case lookupAppls fs aps of
      Just i  -> pure i
      Nothing ->
        case M.lookup fs apsc of
          Just i  -> pure i
          Nothing ->
           do let i = seIx env
              modify (\env -> env { seApplsCur = M.insert fs i apsc , seIx = 1 + i })
              pure i

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


class TheorySymbols a where
  theorySymbols :: a ->  SEnv TheorySymbol

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
  | Defined       -- ^ for user-defined `define-fun`
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
      | setConName == symbol c   = SSet (sortSmtSort poly env a)
    go (FTC c) [a]
      | bagConName == symbol c   = SBag (sortSmtSort poly env a)
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
coerceSortEnv ef ss = coerceSort ef <$> ss

coerceSort :: ElabFlags -> Sort -> Sort
coerceSort ef = (if elabSetBag ef then coerceSetBagToArray else id) . coerceMapToArray

coerceEnv :: ElabFlags -> SymEnv -> SymEnv
coerceEnv slv env =
  SymEnv { seSort     = coerceSortEnv slv (seSort env)
         , seTheory   = seTheory env
         , seData     = seData   env
         , seLits     = seLits   env
         , seAppls    = seAppls  env
         , seApplsCur = seApplsCur env
         , seIx       = seIx     env
         }