{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE PatternGuards              #-}

-- | This module contains the data types, operations and
--   serialization functions for representing Fixpoint's
--   implication (i.e. subtyping) and well-formedness
--   constraints in Haskell. The actual constraint
--   solving is done by the `fixpoint.native` which
--   is written in Ocaml.

module Language.Fixpoint.Types.Sorts (

  -- * Embedding to Fixpoint Types
    Sort (..)
  , Sub (..)
  , FTycon, TCEmb
  , sortFTycon
  , intFTyCon, boolFTyCon, realFTyCon, numFTyCon  -- TODO: hide these

  , intSort, realSort, boolSort, strSort, funcSort, arrowSort
  , listFTyCon
  , isListTC
  , fTyconSymbol, symbolFTycon, fTyconSort
  , fApp, fApp', fAppTC
  , fObj

  , sortSubst
  , splitSortAbs, splitSortArgs, isFunctionSort

  , mkFFun
  ) where

import qualified Data.Binary as B
import           Data.Generics             (Data)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

import           Data.Hashable
import           Data.List                 (foldl')
import           Control.DeepSeq
import           Data.Maybe                (fromMaybe)
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Spans
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ
import qualified Data.HashMap.Strict       as M


newtype FTycon = TC LocSymbol deriving (Eq, Ord, Show, Data, Typeable, Generic)
type TCEmb a   = M.HashMap a FTycon

intFTyCon, boolFTyCon, realFTyCon, funcFTyCon, numFTyCon, strFTyCon, listFTyCon :: FTycon
intFTyCon   = TC $ dummyLoc "int"
boolFTyCon  = TC $ dummyLoc "bool"
realFTyCon  = TC $ dummyLoc "real"
numFTyCon   = TC $ dummyLoc "num"
funcFTyCon  = TC $ dummyLoc "function"
strFTyCon   = TC $ dummyLoc strConName
listFTyCon  = TC $ dummyLoc listConName
arrowFTyCon = TC $ dummyLoc arrowConName

isListConName :: LocSymbol -> Bool
isListConName x = c == listConName || c == listLConName --"List"
  where
    c           = val x

isListTC :: FTycon -> Bool
isListTC (TC z) = isListConName z

fTyconSymbol :: FTycon -> Located Symbol
fTyconSymbol (TC s) = s

symbolFTycon :: LocSymbol -> FTycon
symbolFTycon c
  | isListConName c
  = TC $ fmap (const listConName) c
  | otherwise
  = TC c

fApp :: Sort -> [Sort] -> Sort
fApp = foldl' FApp

fAppTC :: FTycon -> [Sort] -> Sort
fAppTC = fApp . fTyconSort

fApp' :: Sort -> ListNE Sort
fApp' = go []
  where
    go acc (FApp t1 t2) = go (t2 : acc) t1
    go acc t            = t : acc

fObj :: LocSymbol -> Sort
fObj = fTyconSort . TC

sortFTycon :: Sort -> Maybe FTycon
sortFTycon FInt    = Just intFTyCon
sortFTycon FReal   = Just realFTyCon
sortFTycon FNum    = Just numFTyCon
sortFTycon (FTC c) = Just c
sortFTycon _       = Nothing


splitSortArgs :: Sort -> ([Sort], Sort)
splitSortArgs = go []
  where
    go acc (FFunc sx s) = go (sx:acc) s 
    go acc s            = (reverse acc, s)

splitSortAbs :: Sort -> ([Int], Sort)
splitSortAbs = go []
  where
    go acc (FAbs sx s) = go (sx:acc) s 
    go acc s           = (reverse acc, s)

isFunctionSort :: Sort -> Bool
isFunctionSort (FFunc _ _) = True
isFunctionSort _           = False

----------------------------------------------------------------------
------------------------------- Sorts --------------------------------
----------------------------------------------------------------------

data Sort = FInt
          | FReal
          | FNum                 -- ^ numeric kind for Num tyvars
          | FFrac                -- ^ numeric kind for Fractional tyvars
          | FObj  Symbol         -- ^ uninterpreted type
          | FVar  !Int           -- ^ fixpoint type variable
          | FFunc !Sort !Sort      -- ^ type-var arity, in-ts ++ [out-t]
          | FTC   FTycon
          | FApp  !Sort !Sort      -- ^ constructed type
          | FAbs  !Int Sort 
          deriving (Eq, Ord, Show, Data, Typeable, Generic)

mkFFun :: Int -> [Sort] -> Sort 
mkFFun i ss = go i
  where
    go i | i == 0 = foldl1 FApp ss   
    go i = FAbs i (go (i-1))
 
{-@ FFunc :: Nat -> ListNE Sort -> Sort @-}

instance Hashable FTycon where
  hashWithSalt i (TC s) = hashWithSalt i s

instance Hashable Sort

newtype Sub = Sub [(Int, Sort)] deriving (Generic)

instance Fixpoint Sort where
  toFix = toFixSort

toFixSort :: Sort -> Doc
toFixSort (FVar i)      = text "@"   <> parens (toFix i)
toFixSort FInt          = text "int"
toFixSort FReal         = text "real"
toFixSort FFrac         = text "frac"
toFixSort (FObj x)      = toFix x
toFixSort FNum          = text "num"
toFixSort t@(FFunc _ _) = toFixFun 0 t
toFixSort (FTC c)       = toFix c
toFixSort t@(FAbs _ _)  = toFixFAbs t 
toFixSort t@(FApp _ _)  = toFixFApp (fApp' t)

toFixFApp            :: ListNE Sort -> Doc
toFixFApp [t]        = toFixSort t
toFixFApp [FTC c, t]
  | isListTC c       = brackets $ toFixSort t
toFixFApp ts         = parens $ intersperse space (toFixSort <$> ts)

-- TODO: this is represented as it used to be,
-- type variables should be treated more properly now
toFixFAbs :: Sort -> Doc 
toFixFAbs t = toFixFun (length vs) s
  where
    (vs, s) = splitSortAbs t 

toFixFun :: Int -> Sort -> Doc 
toFixFun n t = text "func" <> parens (toFix n <> text ", " <> toFix (ts++[t']))
  where
    (ts, t') = splitSortArgs t

instance Fixpoint FTycon where
  toFix (TC s)       = toFix s

-------------------------------------------------------------------------
-- | Exported Basic Sorts -----------------------------------------------
-------------------------------------------------------------------------

boolSort, intSort, realSort, strSort, funcSort :: Sort
boolSort  = fTyconSort boolFTyCon
strSort   = fTyconSort strFTyCon
intSort   = fTyconSort intFTyCon
realSort  = fTyconSort realFTyCon
funcSort  = fTyconSort funcFTyCon
arrowSort = fTyconSort arrowFTyCon

fTyconSort :: FTycon -> Sort
fTyconSort c
  | c == intFTyCon  = FInt
  | c == realFTyCon = FReal
  | c == numFTyCon  = FNum
  | otherwise       = FTC c

------------------------------------------------------------------------
sortSubst                  :: M.HashMap Symbol Sort -> Sort -> Sort
------------------------------------------------------------------------
sortSubst θ t@(FObj x)    = fromMaybe t (M.lookup x θ)
sortSubst θ (FFunc t1 t2) = FFunc (sortSubst θ t1) (sortSubst θ t2)
sortSubst θ (FApp t1 t2)  = FApp  (sortSubst θ t1) (sortSubst θ t2)
sortSubst _  t            = t


instance B.Binary FTycon
instance B.Binary Sort
instance B.Binary Sub

instance NFData FTycon
instance NFData Sort
instance NFData Sub


instance Monoid Sort where
  mempty            = FObj "any"
  mappend t1 t2
    | t1 == mempty  = t2
    | t2 == mempty  = t1
    | t1 == t2      = t1
    | otherwise     = errorstar $ "mappend-sort: conflicting sorts t1 =" ++ show t1 ++ " t2 = " ++ show t2
