--------------------------------------------------------------------------------
-- | This module contains common functions used in the implementations of
--     Simplifiable Expr in both Interpreter.hs and PLE.hs.
--------------------------------------------------------------------------------

{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns              #-}

{-# OPTIONS_GHC -Wno-name-shadowing    #-}

module Language.Fixpoint.Solver.Simplify (applyBooleanFolding, applyConstantFolding, applySetFolding, isSetPred) where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Smt.Theories
import           Data.Hashable
import qualified Data.HashSet         as S
import qualified Data.Maybe           as Mb


applyBooleanFolding :: Brel -> Expr -> Expr -> Expr
applyBooleanFolding brel e1 e2 =
  case (e1, e2) of
    (ECon (R left), ECon (R right)) ->
      Mb.fromMaybe e (bfR brel left right)
    (ECon (R left), ECon (I right)) ->
      Mb.fromMaybe e (bfR brel left (fromIntegral right))
    (ECon (I left), ECon (R right)) ->
      Mb.fromMaybe e (bfR brel (fromIntegral left) right)
    (ECon (I left), ECon (I right)) ->
      Mb.fromMaybe e (bfI brel left right)
    _ -> if isTautoPred e then PTrue else
           if isContraPred e then PFalse else e
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

    bfR :: Brel -> Double -> Double -> Maybe Expr
    bfR brel left right = if getOp brel left right then Just PTrue else Just PFalse

    bfI :: Brel -> Integer -> Integer -> Maybe Expr
    bfI brel left right = if getOp brel left right then Just PTrue else Just PFalse


-- | Replace constant integer and floating point expressions by constant values
-- where possible.
applyConstantFolding :: Bop -> Expr -> Expr -> Expr
applyConstantFolding bop e1 e2 =
  case (dropECst e1, dropECst e2) of
    (ECon (R left), ECon (R right)) ->
      Mb.fromMaybe e (cfR bop left right)
    (ECon (R left), ECon (I right)) ->
      Mb.fromMaybe e (cfR bop left (fromIntegral right))
    (ECon (I left), ECon (R right)) ->
      Mb.fromMaybe e (cfR bop (fromIntegral left) right)
    (ECon (I left), ECon (I right)) ->
      Mb.fromMaybe e (cfI bop left right)
    (EBin Mod  _   _              , _)  -> e
    (EBin bop1 e11 (dropECst -> ECon (R left)), ECon (R right))
      | bop == bop1 -> maybe e (EBin bop e11) (cfR (rop bop) left right)
      | otherwise   -> e
    (EBin bop1 e11 (dropECst -> ECon (R left)), ECon (I right))
      | bop == bop1 -> maybe e (EBin bop e11) (cfR (rop bop) left (fromIntegral right))
      | otherwise   -> e
    (EBin bop1 e11 (dropECst -> ECon (I left)), ECon (R right))
      | bop == bop1 -> maybe e (EBin bop e11) (cfR (rop bop) (fromIntegral left) right)
      | otherwise   -> e
    (EBin bop1 e11 (dropECst -> ECon (I left)), ECon (I right))
      | bop == bop1 -> maybe e (EBin bop e11) (cfI (rop bop) left right)
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
    cfR bop left right = go (getOp' bop)
      where
        go (Just f) =
          let x = f left right
           in if isNaN x || isInfinite x then Just $ ECon (R x)
              else Nothing
        go Nothing = Nothing

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
      -> maybe e (fromBool . S.null) (evalSetI e2)
    (EApp (EVar s) e1') | s == setMem
      -> maybe e fromBool (S.member <$> getInt e1' <*> evalSetI e2)
                        | s == setEmp
      -> maybe e (fromBool . S.null) (S.difference <$> evalSetI e1' <*> evalSetI e2)
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

