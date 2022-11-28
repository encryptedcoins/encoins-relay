{-# LANGUAGE NumericUnderscores #-}

module Utils.Wait where

import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (MonadIO(..))

waitTime :: MonadIO m => Int -> m ()
waitTime = liftIO . threadDelay . (* 1_000_000)