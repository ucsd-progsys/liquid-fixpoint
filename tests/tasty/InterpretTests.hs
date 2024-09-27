module InterpretTests (tests) where

import Arbitrary ()
import Language.Fixpoint.Types.Refinements (Expr)
import qualified SimplifyInterpreter
import Test.Tasty
  ( TestTree,
    adjustOption,
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
    withOptions tests' =
      adjustOption (\(QuickCheckMaxSize n) -> QuickCheckMaxSize (div n 4)) $
      adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 20))
      tests'

prop_fixpoint :: (Expr -> Expr) -> Expr -> Property
prop_fixpoint f e = f e === f (f e)
