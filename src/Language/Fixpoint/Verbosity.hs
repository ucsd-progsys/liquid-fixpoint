-- | Global verbosity IORef, replacing the one provided by @cmdargs@.
--   Use 'setVerbosity' after option parsing to control all 'whenLoud' /
--   'whenNormal' guards throughout the solver.
module Language.Fixpoint.Verbosity
  ( Verbosity (..)
  , setVerbosity
  , getVerbosity
  , whenLoud
  , whenNormal
  , isNormal
  , isLoud
  ) where

import Control.Monad      (when)
import Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe   (unsafePerformIO)

data Verbosity = Quiet | Normal | Loud
  deriving (Eq, Ord, Show)

{-# NOINLINE verbosityRef #-}
verbosityRef :: IORef Verbosity
verbosityRef = unsafePerformIO (newIORef Normal)

setVerbosity :: Verbosity -> IO ()
setVerbosity = writeIORef verbosityRef

getVerbosity :: IO Verbosity
getVerbosity = readIORef verbosityRef

whenLoud :: IO () -> IO ()
whenLoud act = do
  v <- getVerbosity
  when (v >= Loud) act

whenNormal :: IO () -> IO ()
whenNormal act = do
  v <- getVerbosity
  when (v >= Normal) act

isNormal :: IO Bool
isNormal = (>= Normal) <$> getVerbosity

isLoud :: IO Bool
isLoud = (>= Loud) <$> getVerbosity
