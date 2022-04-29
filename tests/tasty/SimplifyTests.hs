module SimplifyTests (tests) where

import Arbitrary (subexprs)
import Language.Fixpoint.Types.Refinements (Bop (Minus), Constant (I), Expr (..))
import qualified SimplifyInterpreter
import qualified SimplifyPLE
import Test.Tasty
  ( TestTree,
    localOption,
    testGroup,
  )
import Test.Tasty.QuickCheck
  ( Property,
    QuickCheckMaxSize (..),
    QuickCheckTests (..),
    counterexample,
    label,
    testProperty,
  )

tests :: TestTree
tests =
  withOptions $
    testGroup
      "simplify does not increase expression size"
      [ testProperty "PLE" (prop_no_increase SimplifyPLE.simplify'),
        testProperty "Interpreter" (prop_no_increase SimplifyInterpreter.simplify')
      ]
  where
    withOptions tests = localOption (QuickCheckMaxSize 4) (localOption (QuickCheckTests 500) tests)

prop_no_increase :: (Expr -> Expr) -> Expr -> Property
prop_no_increase f e =
  let originalSize = exprSize e
      simplified = f e
      simplifiedSize = exprSize simplified
   in label ("reduced size by " ++ show (originalSize - simplifiedSize)) $
        counterexample
          ( unlines
              [ show simplifiedSize ++ " > " ++ show originalSize,
                "simplified: " ++ show simplified
              ]
          )
          (simplifiedSize <= originalSize)

exprSize :: Expr -> Int
-- Undo the removal of ENeg in @simplify@ so it does not count as increasing the size of the expression.
exprSize (EBin Minus (ECon (I 0)) e) = exprSize (ENeg e)
exprSize e = 1 + sum (exprSize <$> subexprs e)
