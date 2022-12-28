{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wno-name-shadowing    #-}

-- | This module contains the types defining an SMTLIB2 interface.

module Language.Fixpoint.Smt.Types (

    -- * Serialized Representation
    --    symbolBuilder

    -- * Commands
      Command  (..)

    -- * Responses
    , Response (..)

    -- * Typeclass for SMTLIB2 conversion
    , SMTLIB2 (..)
    , runSmt2

    -- * SMTLIB2 Process Context
    , Context (..)
    , ContextHandle (..)

    ) where

import           Language.Fixpoint.Types
import           Language.Fixpoint.Utils.Builder (Builder)
import qualified Data.Text                as T
import           Text.PrettyPrint.HughesPJ
import qualified SMTLIB.Backends as Bck
import qualified SMTLIB.Backends.Process as Process
import qualified SMTLIB.Backends.Z3 as Z3

import           System.IO                (Handle)
-- import           Language.Fixpoint.Misc   (traceShow)

--------------------------------------------------------------------------------
-- | Types ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- symbolBuilder :: Symbol -> LT.Builder
-- symbolBuilder = LT.fromText . symbolSafeText

-- | Commands issued to SMT engine
data Command      = Push
                  | Pop
                  | Exit
                  | SetMbqi
                  | CheckSat
                  | DeclData ![DataDecl]
                  | Declare  T.Text [SmtSort] !SmtSort
                  | Define   !Sort
                  | DefineFunc Symbol [(Symbol, SmtSort)] !SmtSort Expr
                  | Assert   !(Maybe Int) !Expr
                  | AssertAx !(Triggered Expr)
                  | Distinct [Expr] -- {v:[Expr] | 2 <= len v}
                  | GetValue [Symbol]
                  | CMany    [Command]
                  deriving (Eq, Show)

instance PPrint Command where
  pprintTidy _ = ppCmd

ppCmd :: Command -> Doc
ppCmd Exit             = text "Exit"
ppCmd SetMbqi          = text "SetMbqi"
ppCmd Push             = text "Push"
ppCmd Pop              = text "Pop"
ppCmd CheckSat         = text "CheckSat"
ppCmd (DeclData d)     = text "Data" <+> pprint d
ppCmd (Declare x [] t) = text "Declare" <+> text (T.unpack x) <+> text ":" <+> pprint t
ppCmd (Declare x ts t) = text "Declare" <+> text (T.unpack x) <+> text ":" <+> parens (pprint ts) <+> pprint t
ppCmd Define {}   = text "Define ..."
ppCmd (DefineFunc name params rsort e) =
  text "DefineFunc" <+> pprint name <+> pprint params <+> pprint rsort <+> pprint e
ppCmd (Assert _ e)  = text "Assert" <+> pprint e
ppCmd (AssertAx _)  = text "AssertAxiom ..."
ppCmd Distinct {} = text "Distinct ..."
ppCmd GetValue {} = text "GetValue ..."
ppCmd CMany {}    = text "CMany ..."

-- | Responses received from SMT engine
data Response     = Ok
                  | Sat
                  | Unsat
                  | Unknown
                  | Values [(Symbol, T.Text)]
                  | Error !T.Text
                  deriving (Eq, Show)

-- | Represention of the SMT solver backend
data ContextHandle = Process Process.Handle | Z3lib Z3.Handle

-- | Additional information around the SMT solver backend
data Context = Ctx
  {
  -- | The high-level interface for interacting with the SMT solver backend.
    ctxSolver :: Bck.Solver
  -- | The low-level handle for managing the SMT solver backend.
  , ctxHandle :: ContextHandle
  , ctxLog     :: !(Maybe Handle)
  , ctxVerbose :: !Bool
  , ctxSymEnv  :: !SymEnv
  }

--------------------------------------------------------------------------------
-- | AST Conversion: Types that can be serialized ------------------------------
--------------------------------------------------------------------------------

class SMTLIB2 a where
  smt2 :: SymEnv -> a -> Builder

runSmt2 :: (SMTLIB2 a) => SymEnv -> a -> Builder
runSmt2 = smt2
