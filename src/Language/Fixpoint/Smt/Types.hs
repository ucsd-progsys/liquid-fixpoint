{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This module contains the types defining an SMTLIB2 interface.

module Language.Fixpoint.Smt.Types (

    -- * Serialized Representation
      Raw

    -- * Commands
    , Command  (..)

    -- * Responses
    , Response (..)

    -- * Typeclass for SMTLIB2 conversion
    , SMTLIB2 (..)

    -- * SMTLIB2 Process Context
    , Context (..)

    -- * Theory Symbol
    , TheorySymbol (..)

    ) where

import           Language.Fixpoint.Types

import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import           System.IO                (Handle)
import           System.Process

--------------------------------------------------------------------------
-- | Types ---------------------------------------------------------------
--------------------------------------------------------------------------

type Raw          = T.Text

-- | Commands issued to SMT engine
instance Eq (FInfo ())
instance Eq (WfC ())
instance Eq (SubC ())

data Command      = Push
                  | Pop
                  | CheckSat
                  | Declare   Symbol [Sort] Sort
                  | Define    Sort
                  | Assert    (Maybe Int) Pred
                  | forall a. Interpolate (FInfo a) Pred Pred
                  | Distinct  [Expr] -- {v:[Expr] | 2 <= len v}
                  | GetValue  [Symbol]

instance Show Command where
  show Push = "Push"
  show Pop = "Pop"
  show CheckSat = "CheckSat"
  show (Declare a b c) = "Declare " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (Define a) = "Define " ++ show a
  show (Assert a b) = "Assert " ++ show a ++ " " ++ show b
  show (Interpolate _ b c) = "Interpolate fi " ++ show b ++ show c
  show (Distinct a) = "Distinct " ++ show a
  show (GetValue a) = "GetValue " ++ show a

-- | Responses received from SMT engine
data Response     = Ok
                  | Sat
                  | Unsat
                  | Unknown
                  | Values [(Symbol, Raw)]
                  | Interpolant Pred
                  | Error Raw
                  deriving (Eq, Show)

-- | Information about the external SMT process
data Context      = Ctx { pId     :: ProcessHandle
                        , cIn     :: Handle
                        , cOut    :: Handle
                        , cLog    :: Maybe Handle
                        , verbose :: Bool
                        }

-- | Theory Symbol
data TheorySymbol  = Thy { tsSym  :: Symbol
                         , tsRaw  :: Raw
                         , tsSort :: Sort
                         }
                     deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- | AST Conversion: Types that can be serialized ---------------------
-----------------------------------------------------------------------

-- | Types that can be serialized
class SMTLIB2 a where
  smt2 :: a -> LT.Text

