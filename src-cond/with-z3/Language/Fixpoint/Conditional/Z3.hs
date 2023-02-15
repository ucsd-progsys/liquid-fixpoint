module Language.Fixpoint.Conditional.Z3 where

import qualified SMTLIB.Backends
import qualified SMTLIB.Backends.Z3 as Z3

makeZ3 :: IO (SMTLIB.Backends.Backend, IO ())
makeZ3 = do
    handle <- Z3.new Z3.defaultConfig
    return (Z3.toBackend handle, Z3.close handle)

builtWithZ3AsALibrary :: Bool
builtWithZ3AsALibrary = True
