{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

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

    -- * Strict Formatter
    , format

    ) where

import           Language.Fixpoint.Types

import qualified Data.Text.Format         as DTF
import           Data.Text.Format.Params  (Params)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import           System.IO                (Handle)
import           System.Process

--------------------------------------------------------------------------
-- | Types ---------------------------------------------------------------
--------------------------------------------------------------------------

type Raw          = T.Text

-- | Commands issued to SMT engine
data Command      = Push
                  | Pop
                  | CheckSat
                  | Declare   Symbol [Sort] Sort
                  | Define    Sort
                  | Assert    (Maybe Int) Pred
                  | Distinct  [Expr] -- {v:[Expr] | 2 <= len v}
                  | GetValue  [Symbol]
                  deriving (Eq, Show)

-- | Responses received from SMT engine
data Response     = Ok
                  | Sat
                  | Unsat
                  | Unknown
                  | Values [(Symbol, Raw)]
                  | Error Raw
                  deriving (Eq, Show)

-- | Information about the external SMT process
data Context      = Ctx { pId     :: ProcessHandle
                        , cIn     :: Handle
                        , cOut    :: Handle
                        , cLog    :: Maybe Handle
                        , verbose :: Bool

                        -- debug statistics
                        , nBrkt   :: !Integer
                        , nAsst   :: !Integer
                        , nChck   :: !Integer
                        , nUnst   :: !Integer
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

format :: Params ps => DTF.Format -> ps -> T.Text
format f x = LT.toStrict $ DTF.format f x

-- | Types that can be serialized
class SMTLIB2 a where
  smt2 :: a -> T.Text
