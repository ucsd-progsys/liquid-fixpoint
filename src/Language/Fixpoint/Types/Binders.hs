module Language.Fixpoint.Types.Binders where

import Data.Hashable (Hashable)

class (Eq b, Ord b, Hashable b) => Binder b where
  wildcard :: b
  editDistance :: b -> b -> Int
  editDistance b1 b2
    | b1 == b2  = 0
    | otherwise = maxBound
