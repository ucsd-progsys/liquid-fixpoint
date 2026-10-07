{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}

module Language.Fixpoint.Types.Triggers (

    Triggered (..), Trigger(..),

    noTrigger,

    triggerPatterns

    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Data                 (Data)
import qualified Data.Store as S
import           Control.DeepSeq
import           GHC.Generics              (Generic)
import           Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Types.Refinements
import Language.Fixpoint.Types.PrettyPrint


data Triggered a = TR Trigger a
  deriving (Eq, Show, Data, Functor, Generic)

newtype Trigger
  = Patterns [[Expr]]   -- ^ explicit (multi-)patterns, one list per SMTLIB @:pattern@;
                        --   empty means the expression is asserted without patterns
  deriving (Eq, Show, Data, Generic)

instance PPrint Trigger where
  pprintTidy _ = text . show

instance PPrint a => PPrint (Triggered a) where
  pprintTidy k (TR t x) = parens (pprintTidy k t <+> text ":" <+> pprintTidy k x)

noTrigger :: e -> Triggered e
noTrigger = TR (Patterns [])

-- | The SMTLIB @:pattern@s of a triggered expression; each element is a multi-pattern.
triggerPatterns :: Triggered Expr -> [[Expr]]
triggerPatterns (TR (Patterns ps) _) = ps

instance S.Store Trigger
instance NFData   Trigger
instance ToJSON   Trigger
instance FromJSON Trigger

instance (S.Store a) => S.Store (Triggered a)
instance (NFData a)   => NFData   (Triggered a)
instance (ToJSON a)   => ToJSON   (Triggered a)
instance (FromJSON a) => FromJSON (Triggered a)
