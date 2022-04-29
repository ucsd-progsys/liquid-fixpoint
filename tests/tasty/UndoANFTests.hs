{-# LANGUAGE OverloadedStrings #-}

module UndoANFTests(tests) where

import Language.Fixpoint.Types (SortedReft(..), Sort, Reft(..), Symbol, Expr(..),
                                reft, isPrefixOfSym, anfPrefix, syms)
import Language.Fixpoint.Solver.EnvironmentReduction (undoANFSimplifyingWith)
import qualified Language.Fixpoint.Types.Visitor as Visitor
import Arbitrary
import qualified Data.HashMap.Strict as M
import Test.Tasty (TestTree, testGroup, adjustOption, localOption)
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as H
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as Q


tests :: TestTree
tests =
  withOptions $
    testGroup
      "undoANFSimplifyingWith id id"
      [ H.testCase "id on empty env" $
          simpleUndoANF [] @?= M.empty
      , Q.testProperty "id when env contains no lq_anf$* bindings" $
          prop_no_change (M.fromList . unEnv . unNoAnfEnv) simpleUndoANFNoAnfEnv
      , testGroup
          "zero anf vars left afterwards, starting with:"
          [ Q.testProperty "no anf vars" $
              prop_no_anfs simpleUndoANFNoAnfEnv
          , Q.testProperty "single-level anf vars" $
              prop_no_anfs simpleUndoANFFlatAnfEnv
          , Q.testProperty "chained anf vars" $
              prop_no_anfs simpleUndoANFChainedAnfEnv
          ]
      ]
  where
    withOptions = adjustOption (min (Q.QuickCheckMaxSize 8))   -- adjustOption . min because we don't want to default to the enormous value.
                  . adjustOption (max (Q.QuickCheckTests 500)) -- adjustOption . max because we may want larger on the command line.

-- | 5 seconds (in microseconds).
timeout :: Int
timeout = 5000000

prop_no_change :: (Q.Arbitrary e, Eq e, Show e) => (e -> M.HashMap Symbol SortedReft) -> (e -> M.HashMap Symbol SortedReft) -> e -> Q.Property
prop_no_change toHashMap f e = Q.within timeout $ f e === toHashMap e

prop_no_anfs :: (Q.Arbitrary e, Eq e, Show e) => (e -> M.HashMap Symbol SortedReft) -> e -> Q.Property
prop_no_anfs f e = Q.within timeout . checkNoAnfs . f $ e
  where
    checkNoAnfs m = M.filter (any isAnfVar . syms) m === M.empty
    isAnfVar = isPrefixOfSym anfPrefix

-- | We perform tests with only trivial lenses (i.e. id)
simpleUndoANF :: [(Symbol, SortedReft)] -> M.HashMap Symbol SortedReft
simpleUndoANF = undoANFSimplifyingWith id id . M.fromList

----------------------------------------------------
-- | simpleUndoANF conjugated with various newtypes
----------------------------------------------------

simpleUndoANFEnv :: Env -> M.HashMap Symbol SortedReft
simpleUndoANFEnv = simpleUndoANF . unEnv

simpleUndoANFNoAnfEnv :: NoAnfEnv -> M.HashMap Symbol SortedReft
simpleUndoANFNoAnfEnv = simpleUndoANFEnv . unNoAnfEnv

simpleUndoANFFlatAnfEnv :: FlatAnfEnv -> M.HashMap Symbol SortedReft
simpleUndoANFFlatAnfEnv = simpleUndoANFEnv . unFlatAnfEnv

simpleUndoANFChainedAnfEnv :: ChainedAnfEnv -> M.HashMap Symbol SortedReft
simpleUndoANFChainedAnfEnv = simpleUndoANFEnv . unChainedAnfEnv
