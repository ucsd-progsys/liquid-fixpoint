{-# LANGUAGE CPP #-}
module Control.Exception.Compat
  ( ExceptionWithContext(..)
  , displayExceptionContext
  , wrapExceptionWithContext
  ) where

#if MIN_VERSION_base(4,20,0)

import Control.Exception (ExceptionWithContext(..))
import Control.Exception.Context (displayExceptionContext)

wrapExceptionWithContext :: ExceptionWithContext a -> ExceptionWithContext a
wrapExceptionWithContext = id

#else

data ExceptionWithContext a = ExceptionWithContext ExceptionContext a

data ExceptionContext = ExceptionContext

displayExceptionContext :: ExceptionContext -> String
displayExceptionContext _ = "Exception context not available in this version of ghc."

wrapExceptionWithContext :: a -> ExceptionWithContext a
wrapExceptionWithContext = ExceptionWithContext ExceptionContext
#endif

