-- | Progress Bar API
module Language.Fixpoint.Utils.Progress (
      withProgress
    , withProgressM
    , progressInit
    , progressTick
    , progressClose
    ) where

withProgress :: Int -> IO a -> IO a
withProgress _ x = x

withProgressM :: (m a -> IO b) -> Int -> m a -> IO b
withProgressM f _ = f

progressInit :: Int -> IO ()
progressInit _ = return ()

progressTick :: IO ()
progressTick = return ()

progressClose :: IO ()
progressClose = return ()
