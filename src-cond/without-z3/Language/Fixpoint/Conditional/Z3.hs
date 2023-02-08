module Language.Fixpoint.Conditional.Z3 where

makeZ3 :: IO a
makeZ3 = error "liquid-fixpoint: Not built with the Z3 backend. Please, enable the cabal flag link-z3-as-a-library."

builtWithZ3AsALibrary :: Bool
builtWithZ3AsALibrary = False
