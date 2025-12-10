
module SimplifyKVarTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "simplifyKVar"
    [ testCase "Disabled because it needs MultilineStrings (ghc >= 9.12.1)" $
        return ()
    ]
