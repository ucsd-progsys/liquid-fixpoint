{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
          simpleUndoANF M.empty @?= M.empty
      , Q.testProperty "id when env contains no lq_anf$* bindings" $
          prop_no_change simpleUndoANFNoAnfEnv
      , testGroup
          "zero anf vars left afterwards, starting with:"
          [ Q.testProperty "no anf vars" $
              prop_no_anfs unNoAnfEnv simpleUndoANFNoAnfEnv
          , Q.testProperty "single-level anf vars" $
              prop_no_anfs unFlatAnfEnv simpleUndoANFFlatAnfEnv
          , Q.testProperty "chained anf vars" $
              prop_no_anfs unChainedAnfEnv simpleUndoANFChainedAnfEnv
          ]
      ]
  where
    withOptions = localOption (Q.QuickCheckMaxSize 8) -- localOption because default value is larger than 8.
                  . adjustOption (max (Q.QuickCheckTests 500)) -- adjustOption . max because we may want larger on the command line.

-- | 5 seconds (in microseconds).
timeout :: Int
timeout = 5000000

prop_no_change :: (Q.Arbitrary e, Eq e, Show e) => (e -> e) -> e -> Q.Property
prop_no_change f e = Q.within timeout $ f e === e

prop_no_anfs :: (Q.Arbitrary e, Eq e, Show e) => (e -> Env) -> (e -> e) -> e -> Q.Property
prop_no_anfs toEnv f e = Q.within timeout . checkNoAnfs . toEnv . f $ e
  where
    checkNoAnfs (Env m) = M.filter (any isAnfVar . syms) m === M.empty
    isAnfVar = isPrefixOfSym anfPrefix

-- | We perform tests with only trivial lenses (i.e. id)
simpleUndoANF :: M.HashMap Symbol SortedReft -> M.HashMap Symbol SortedReft
simpleUndoANF = undoANFSimplifyingWith id id

----------------------------------------------------
-- | simpleUndoANF conjugated with various newtypes
----------------------------------------------------

simpleUndoANFEnv :: Env -> Env
simpleUndoANFEnv = Env . simpleUndoANF . unEnv

simpleUndoANFNoAnfEnv :: NoAnfEnv -> NoAnfEnv
simpleUndoANFNoAnfEnv = NoAnfEnv . simpleUndoANFEnv . unNoAnfEnv

simpleUndoANFFlatAnfEnv :: FlatAnfEnv -> FlatAnfEnv
simpleUndoANFFlatAnfEnv = FlatAnfEnv . simpleUndoANFEnv . unFlatAnfEnv

simpleUndoANFChainedAnfEnv :: ChainedAnfEnv -> ChainedAnfEnv
simpleUndoANFChainedAnfEnv = ChainedAnfEnv . simpleUndoANFEnv . unChainedAnfEnv
