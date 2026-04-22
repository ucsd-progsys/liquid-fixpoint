{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveDataTypeable         #-}

{-# OPTIONS_GHC -Wno-name-shadowing     #-}

-- | This module contains the top-level SOLUTION data types,
--   including various indices used for solving.

module Language.Fixpoint.Types.Solutions (

  -- * Solution tables
    Solution
  , Sol (..)
  , CMap

  -- * Solution elements
  , Hyp, Cube (..), QBind (..)
  , EQual (..)

  -- * Equal elements
  , eQual
  , trueEqual

  , qbExprs

  -- * Solution Candidates (move to SolverMonad?)
  , Cand

  -- * Update
  , update

  -- * Lookup
  , lookupQBind
  , lookup

  -- * Manipulating QBind
  , qb
  , qbFilter
  , qbFilterM


  -- * Conversion for client
  , result

  -- * "Fast" Solver (DEPRECATED as unsound)
  , Index  (..)
  , KIndex (..)
  , BindPred (..)
  , BIndex (..)
  ) where

import           Prelude hiding (lookup)
import           GHC.Generics
import           Control.DeepSeq
import           Data.Hashable
import qualified Data.Maybe                 as Mb
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L
import           Data.Generics             (Data)
import           Data.Typeable             (Typeable)
import           Control.Monad (filterM)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Spans
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Refinements
import           Language.Fixpoint.Types.Environments
import           Language.Fixpoint.Types.Constraints
import           Language.Fixpoint.Types.Substitutions
import           Text.PrettyPrint.HughesPJ.Compat

--------------------------------------------------------------------------------
-- | Update Solution
--
-- @update s kqs@ sets in @s@ each KVar in @kqs@ to the corresponding QBind.
--
-- Yields a pair @(b, s')@ where @b@ is true if the mapping of any KVar was
-- changed.
--
-- Precondition: @kqs@ contains no duplicate KVars.
--
update :: Sol QBind -> [(KVar, QBind)] -> (Bool, Sol QBind)
--------------------------------------------------------------------------------
update s kqs = L.foldl' step (False, s) kqs
  where
    step :: (Bool, Sol QBind) -> (KVar, QBind) -> (Bool, Sol QBind)
    step (changed, s) (k, qs) = (changed || distinctSizes, updateK k qs s)
      where
        oldQs = lookupQBind s k
        distinctSizes = qbSize oldQs /= qbSize qs

--------------------------------------------------------------------------------
-- | The `Solution` data type --------------------------------------------------
--------------------------------------------------------------------------------
type Solution  = Sol QBind
newtype QBind  = QB [EQual]   deriving (Show, Data, Typeable, Generic, Eq)

qb :: [EQual] -> QBind
qb = QB

qbEQuals :: QBind -> [EQual]
qbEQuals (QB xs) = xs

qbExprs :: QBind -> [Expr]
qbExprs (QB xs) = eqPred <$> xs

qbSize :: QBind -> Int
qbSize = length . qbEQuals

qbFilter :: (EQual -> Bool) -> QBind -> QBind
qbFilter f (QB eqs) = QB (filter f eqs)

qbFilterM :: Monad m => (EQual -> m Bool) -> QBind -> m QBind
qbFilterM f (QB eqs) = QB <$> filterM f eqs

instance NFData QBind

instance PPrint QBind where
  pprintTidy k = pprintTidy k . qbEQuals

--------------------------------------------------------------------------------
-- | A `Sol` contains the various indices needed to compute a solution,
--   in particular, to compute `lhsPred` for any given constraint.
--------------------------------------------------------------------------------
data Sol a = Sol
  { sMap :: !(M.HashMap KVar a)          -- ^ Actual solution (for cut kvar)
  , sHyp :: !(M.HashMap KVar Hyp)        -- ^ Defining cubes  (for non-cut kvar)
  , sScp :: !(M.HashMap KVar IBindEnv)   -- ^ Set of binders which are in scope for every
                                         -- occurrence of the kvar
  } deriving (Generic)

deriving instance NFData a => NFData (Sol a)

instance Semigroup (Sol a) where
  s1 <> s2 = Sol { sMap  = sMap s1  <> sMap s2
                 , sHyp  = sHyp s1  <> sHyp s2
                 , sScp  = sScp s1  <> sScp s2
                 }

instance Monoid (Sol a) where
  mempty = Sol { sMap = mempty
               , sHyp = mempty
               , sScp = mempty
               }
  mappend = (<>)

instance Functor Sol where
  fmap f (Sol s m1 m2) = Sol (f <$> s) m1 m2

instance PPrint a => PPrint (Sol a) where
  pprintTidy k s = vcat [ "sMap :=" <+> pprintTidy k (sMap s) ]

--------------------------------------------------------------------------------
-- | A `Cube` is a single constraint defining a KVar ---------------------------
--------------------------------------------------------------------------------
type Hyp      = ListNE Cube

data Cube = Cube
  { cuBinds :: IBindEnv  -- ^ Binders       from defining Env
  , cuSubst :: Subst     -- ^ Substitutions from cstrs    Rhs
  , cuId    :: SubcId    -- ^ Id            of   defining Cstr
  , cuTag   :: Tag       -- ^ Tag           of   defining Cstr (DEBUG)
  } deriving (Generic, NFData)

instance PPrint Cube where
  pprintTidy _ c = "Cube" <+> pprint (cuId c)

instance Show Cube where
  show = showpp
--------------------------------------------------------------------------------
result :: Sol QBind -> M.HashMap KVar Expr
--------------------------------------------------------------------------------
result s = pAnd . fmap eqPred . qbEQuals <$> sMap s

--------------------------------------------------------------------------------
-- | Read / Write Solution at KVar ---------------------------------------------
--------------------------------------------------------------------------------
lookupQBind :: Sol QBind -> KVar -> QBind
--------------------------------------------------------------------------------
lookupQBind s k = {- tracepp _msg $ -} Mb.fromMaybe (QB []) (lookupElab s k)
  where
    _msg        = "lookupQB: k = " ++ show k

--------------------------------------------------------------------------------
lookup :: Sol QBind -> KVar -> Either Hyp QBind
--------------------------------------------------------------------------------
lookup s k
  | Just cs  <- M.lookup k (sHyp s) -- non-cut variable, return its cubes
  = Left cs
  | Just eqs <- lookupElab s k
  = Right eqs                 -- TODO: don't initialize kvars that have a hyp solution
  | otherwise
  = errorstar $ "solLookup: Unknown kvar " ++ show k

lookupElab :: Sol QBind -> KVar -> Maybe QBind
lookupElab s k = M.lookup k (sMap s)

--------------------------------------------------------------------------------
updateK :: KVar -> a -> Sol a -> Sol a
--------------------------------------------------------------------------------
updateK k qs s = s { sMap = M.insert k qs (sMap s)
--                 , sBot = M.delete k    (sBot s)
                   }


--------------------------------------------------------------------------------
-- | A `Cand` is an association list indexed by predicates
--------------------------------------------------------------------------------
type Cand a   = [(Expr, a)]


--------------------------------------------------------------------------------
-- | Instantiated Qualifiers ---------------------------------------------------
--------------------------------------------------------------------------------
data EQual = EQL
  { eqQual :: !Qualifier
  , eqPred  :: !Expr      -- ^ predicate obtained by instantiating the qualifier
  , _eqArgs :: ![Expr]    -- ^ actual arguments used to instantiate the qualifier
  } deriving (Eq, Show, Data, Typeable, Generic)

instance Loc EQual where
  srcSpan = srcSpan . eqQual

trueEqual :: EQual
trueEqual = EQL trueQual PTrue []

instance PPrint EQual where
  pprintTidy k = pprintTidy k . eqPred

instance NFData EQual

-- | @eQual q xs@ instantiates @q@ with the arguments in @xs@
eQual :: Qualifier -> [Symbol] -> EQual
eQual q xs = {- tracepp "eQual" $ -} EQL q p es
  where
    p      = subst su $  qBody q
    su     = mkSubst  $  safeZip "eQual" qxs es
    es     = eVar    <$> xs
    qxs    = qpSym   <$> qParams q

--------------------------------------------------------------------------------
-- | A KIndex uniquely identifies each *use* of a KVar in an (LHS) binder
--------------------------------------------------------------------------------
data KIndex = KIndex { kiBIndex :: !BindId
                     , kiPos    :: !Int
                     , kiKVar   :: !KVar
                     }
              deriving (Eq, Ord, Show, Generic)

instance Hashable KIndex

instance PPrint KIndex where
  pprintTidy _ = tshow

--------------------------------------------------------------------------------
-- | A BIndex is created for each LHS Bind or RHS constraint
--------------------------------------------------------------------------------
data BIndex    = Root
               | Bind !BindId
               | Cstr !SubcId
                 deriving (Eq, Ord, Show, Generic)

instance Hashable BIndex

instance PPrint BIndex where
  pprintTidy _ = tshow

--------------------------------------------------------------------------------
-- | Each `Bind` corresponds to a conjunction of a `bpConc` and `bpKVars`
--------------------------------------------------------------------------------
data BindPred  = BP
  { bpConc :: !Pred                  -- ^ Concrete predicate (PTrue o)
  , bpKVar :: ![KIndex]              -- ^ KVar-Subst pairs
  } deriving (Show)

instance PPrint BindPred where
  pprintTidy _ = tshow


--------------------------------------------------------------------------------
-- | A Index is a suitably indexed version of the cosntraints that lets us
--   1. CREATE a monolithic "background formula" representing all constraints,
--   2. ASSERT each lhs via bits for the subc-id and formulas for dependent cut KVars
--------------------------------------------------------------------------------
data Index = FastIdx
  { bindExpr   :: !(BindId |-> BindPred) -- ^ BindPred for each BindId
  , kvUse      :: !(KIndex |-> KVSub)    -- ^ Definition of each `KIndex`
  , kvDef      :: !(KVar   |-> Hyp)      -- ^ Constraints defining each `KVar`
  , envBinds   :: !(CMap IBindEnv)       -- ^ Binders of each Subc
  , envTx      :: !(CMap [SubcId])       -- ^ Transitive closure oof all dependent binders
  , envSorts   :: !(SEnv Sort)           -- ^ Sorts for all symbols
  -- , bindPrev   :: !(BIndex |-> BIndex)   -- ^ "parent" (immediately dominating) binder
  -- , kvDeps     :: !(CMap [KIndex])       -- ^ List of (Cut) KVars on which a SubC depends
  }

type CMap a  = M.HashMap SubcId a
