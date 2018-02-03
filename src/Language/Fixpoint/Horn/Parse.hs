
-- | A function to parse queries in Horn format .smt2 files

{-# LANGUAGE FlexibleInstances #-}

module Language.Fixpoint.Horn.Parse () where

import Language.Fixpoint.Parse
import Language.Fixpoint.Horn.Types

instance Inputable (Horn Int) where
  rr' = doParse' hornP

hornP :: Parser (Horn Int)
hornP = error "_fixme_hornParser"
