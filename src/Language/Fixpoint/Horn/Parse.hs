
-- | A function to parse queries in Horn format .smt2 files

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}


module Language.Fixpoint.Horn.Parse () where

import           GHC.Generics              (Generic)
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Parse as F
import           Language.Fixpoint.Horn.Types

instance F.Inputable (Horn Int) where
  rr' = F.doParse' hornP

hornP :: F.Parser (Horn Int)
hornP = error "_fixme_hornParser"

data Def a
  = HdQual !F.Qualifier
  | HdWfc  !(HornWf   a)
  | HdCstr !(HornCstr a)
  deriving (Generic, Show, Functor)

exprP

qualP :: F.Parser (HornWf Int)
qualP = error "fixme_hornWfP"

wfP :: F.Parser (HornWf Int)
wfP = error "fixme_hornWfP"

cstrP :: F.Parser (HornCstr Int)
cstrP = error "fixme_hornWfP"
