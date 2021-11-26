module InterpretTests (tests) where

import Arbitrary ()
import Language.Fixpoint.Types.Refinements (Expr (..))
import qualified SimplifyInterpreter
import Test.Tasty
  ( TestTree,
    localOption,
    testGroup,
  )
import Test.Tasty.QuickCheck
  ( Property,
    QuickCheckMaxSize (..),
    QuickCheckTests (..),
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  withOptions $
    testGroup
      "interpret"
      [ testProperty "computes a fixpoint" (prop_fixpoint SimplifyInterpreter.interpret')
      ]
  where
    withOptions tests = localOption (QuickCheckMaxSize 4) (localOption (QuickCheckTests 500) tests)

prop_fixpoint :: (Expr -> Expr) -> Expr -> Property
prop_fixpoint f e = f e === f (f e)
