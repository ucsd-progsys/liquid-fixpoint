--------------------------------------------------------------------------------
-- | This module contains common functions used in the implementations of
--     Simplifiable Expr in both Interpreter.hs and PLE.hs.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Fixpoint.Solver.Simplify (applyBooleanFolding, applyConstantFolding, applySetFolding, isSetPred) where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Smt.Theories
import           Data.Hashable
import qualified Data.HashSet         as S
import qualified Data.Maybe           as Mb


applyBooleanFolding :: Brel -> Expr -> Expr -> Expr
applyBooleanFolding brel e1 e2 =
  case (e1, e2) of
    (ECon (R left), ECon (R right)) -> bfR brel left right
    (ECon (R left), ECon (I right)) -> bfR brel left (fromIntegral right)
    (ECon (I left), ECon (R right)) -> bfR brel (fromIntegral left) right
    (ECon (I left), ECon (I right)) -> bfI brel left right
    _ | isTautoPred e -> PTrue
    _ | isContraPred e -> PFalse
    _ -> e
  where
    e = PAtom brel e1 e2

    getOp :: Ord a => Brel -> (a -> a -> Bool)
    getOp Gt   =  (>)
    getOp Ge   =  (>=)
    getOp Lt   =  (<)
    getOp Le   =  (<=)
    getOp Eq   =  (==)
    getOp Ne   =  (/=)
    getOp Ueq  =  (==)
    getOp Une  =  (/=)

    pBool :: Bool -> Expr
    pBool True = PTrue
    pBool False = PFalse

    bfR :: Brel -> Double -> Double -> Expr
    bfR brel left right = pBool $ getOp brel left right

    bfI :: Brel -> Integer -> Integer -> Expr
    bfI brel left right = pBool $ getOp brel left right


applyConstantFolding :: Bop -> Expr -> Expr -> Expr
applyConstantFolding bop e1 e2 =
  case (e1, e2) of
    ((ECon (R left)), (ECon (R right))) ->
      Mb.fromMaybe e (cfR bop left right)
    ((ECon (R left)), (ECon (I right))) ->
      Mb.fromMaybe e (cfR bop left (fromIntegral right))
    ((ECon (I left)), (ECon (R right))) ->
      Mb.fromMaybe e (cfR bop (fromIntegral left) right)
    ((ECon (I left)), (ECon (I right))) ->
      Mb.fromMaybe e (cfI bop left right)
    (EBin Mod  _   _              , _)  -> e
    (EBin bop1 e11 (ECon (R left)), ECon (R right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfR (rop bop) left right))
      | otherwise   -> e
    (EBin bop1 e11 (ECon (R left)), ECon (I right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfR (rop bop) left (fromIntegral right)))
      | otherwise   -> e
    (EBin bop1 e11 (ECon (I left)), ECon (R right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfR (rop bop) (fromIntegral left) right))
      | otherwise   -> e
    (EBin bop1 e11 (ECon (I left)), ECon (I right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfI (rop bop) left right))
      | otherwise   -> e
    _ -> e
  where

    rop :: Bop -> Bop
    rop Plus   = Plus
    rop Minus  = Plus
    rop Times  = Times
    rop Div    = Times
    rop RTimes = RTimes
    rop RDiv   = RTimes
    rop Mod    = Mod

    e = EBin bop e1 e2

    getOp :: Num a => Bop -> Maybe (a -> a -> a)
    getOp Minus    = Just (-)
    getOp Plus     = Just (+)
    getOp Times    = Just (*)
    getOp RTimes   = Just (*)
    getOp _        = Nothing

    cfR :: Bop -> Double -> Double -> Maybe Expr
    cfR bop left right = fmap go (getOp' bop)
      where
        go f = ECon $ R $ f left right

        getOp' Div      = Just (/)
        getOp' RDiv     = Just (/)
        getOp' op       = getOp op

    cfI :: Bop -> Integer -> Integer -> Maybe Expr
    cfI bop left right = fmap go (getOp' bop)
      where
        go f = ECon $ I $ f left right

        getOp' Mod = Just mod
        getOp' op  = getOp op

isSetPred :: Expr -> Bool
isSetPred (EVar s) | s == setEmp          = True
isSetPred (EApp e1 _) = case e1 of
  (EVar s) | s == setMem || s == setSub  -> True
  _                                      -> False
isSetPred _                               = False

-- Note: this is currently limited to sets of integer constants
applySetFolding :: Expr -> Expr -> Expr
applySetFolding e1 e2   = case e1 of
    (EVar s) | s == setEmp
      -> Mb.fromMaybe e $ pure (fromBool . S.null)   <*> evalSetI e2
    (EApp (EVar s) e1') | s == setMem
      -> Mb.fromMaybe e $ fromBool <$> (S.member <$> getInt e1' <*> evalSetI e2)
                        | s == setEmp
      -> Mb.fromMaybe e $ fromBool <$> (S.null <$> (S.difference <$> evalSetI e1' <*> evalSetI e2))
                        | otherwise
      -> e
    _                   -> e
  where
    e = EApp e1 e2

    fromBool True  = PTrue
    fromBool False = PFalse

    getInt :: Expr -> Maybe Integer
    getInt (ECon (I n)) = Just n
    getInt _            = Nothing

    getOp :: (Eq a, Hashable a) => Symbol -> Maybe (S.HashSet a -> S.HashSet a -> S.HashSet a)
    getOp s | s == setCup = Just S.union
            | s == setCap = Just S.intersection
            | s == setDif = Just S.difference
            | otherwise   = Nothing

    evalSetI :: Expr -> Maybe (S.HashSet Integer)
    evalSetI (EApp e1 e2) = case e1 of
      (EVar s) | s == setEmpty -> Just S.empty
               | s == setSng   -> case e2 of
        (ECon (I n))             -> Just $ S.singleton n
        _                        -> Nothing
      (EApp (EVar f) e1')  -> getOp f <*> evalSetI e1' <*> evalSetI e2
      _                    -> Nothing
    evalSetI _            = Nothing

