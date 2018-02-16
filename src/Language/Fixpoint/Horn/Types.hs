-- | This module contains the code for representing FQ constraints in Horn format;
--   see tests/pos/*.smt2 for examples.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Fixpoint.Horn.Types
  ( -- * Data type
    HornWf (..)
  , HornCstr (..)
  , Horn (..)
  )
  where

import           GHC.Generics              (Generic)
import           Language.Fixpoint.Types as F

-- | 'HornWf' defines the sorts of the parameters for a given KVar
data HornWf   a = HW
  { hwName   :: !F.KVar       -- ^ name of the KVar
  , hwParams :: ![F.Sort]     -- ^ parameter (sorts) of the KVar
  }
  deriving (Show, Generic, Functor)

-- | 'HornCstr' defines a *nested* Horn constraint (cf. [Cosman & Jhala, ICFP 2017]
data HornCstr a
  = CHead !F.Pred a                                   -- ^ p
  | CAnd  ![HornCstr a]                               -- ^ c1 /\ ... /\ cn
  | CBind !F.Symbol !F.Sort !F.Pred !(HornCstr a)     -- ^ forall x:b. p => c
  deriving (Show, Generic, Functor)

-- | 'Horn' defines a full query comprising 'HornWf' and 'HornCstr'
data Horn a = Horn
  { hnQuals :: ![F.Qualifier]       -- ^ qualifiers to use for solving
  , hnWfs   :: ![HornWf    a]       -- ^ sort-definitions for KVars
  , hnCstrs :: ![HornCstr  a]       -- ^ nested horn constraints
  }
  deriving (Show, Generic, Functor)
