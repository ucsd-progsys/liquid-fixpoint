module Language.Fixpoint.Utils.Profile where

import Data.Time.Clock
import Control.Monad
import Control.Monad.IO.Class

profileIO :: MonadIO m => Bool -> String -> m a -> m a
profileIO shouldProfile header m = do
  beforeTime <- liftIO getCurrentTime
  ret <- m
  afterTime <- liftIO $ getCurrentTime
  when shouldProfile $ liftIO $ putStrLn $ header <> show (afterTime `diffUTCTime` beforeTime)
  return ret
