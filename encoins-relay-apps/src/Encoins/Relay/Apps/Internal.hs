{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Encoins.Relay.Apps.Internal where

import           Cardano.Api                        (NetworkId, writeFileJSON)
import           Control.Concurrent                 (threadDelay)
import           Control.Exception                  (AsyncException (UserInterrupt), Exception (..), SomeException)
import           Control.Monad                      (forM)
import           Control.Monad.Catch                (MonadCatch, MonadThrow (throwM), handle, try)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Data.Aeson                         (FromJSON (parseJSON), ToJSON, eitherDecodeFileStrict)
import           Data.Aeson.Types                   (parseMaybe)
import           Data.Maybe                         (catMaybes)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time                          (getCurrentTime)
import           Ledger                             (Slot (getSlot))
import           PlutusAppsExtra.IO.ChainIndex.Kupo (IsValidRequest, KupoRequest (..), getKupoResponse)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), kupoResponseToJSON)

-- getResponses :: MonadIO m => NetworkId -> Maybe Slot -> Maybe Slot -> m [KupoResponse]
-- getResponses networkId slotFrom slotTo = do
    -- _

-- Partially get large kupo responses, with intermediate saving of the result.
partiallyGet :: forall su b a m. (MonadIO m, MonadCatch m, IsValidRequest su b a)
    => NetworkId -> (Text -> m ()) -> Slot -> Slot -> Slot -> KupoRequest su b a -> FilePath -> m [KupoResponse]
partiallyGet newtworkId ((. T.pack) -> mkLog) slotFrom slotTo slotDelta req fp = do
    let intervals = divideTimeIntoIntervals slotFrom slotTo slotDelta
    resValue <- fmap concat $ forM (zip [1 :: Int ..] intervals) $ \(i, (f, t)) -> reloadHandler $ do
        mkLog $ show i <> "/" <> show (length intervals)
        let fileName = fp <> show (getSlot f) <> "_"  <> show (getSlot t) <> ".json"
            getResponse = liftIO $ fmap (kupoResponseToJSON newtworkId) <$> do
                let req' = req{reqCreatedOrSpentAfter = Just f, reqCreatedOrSpentBefore = Just t} :: KupoRequest su b a
                getKupoResponse req'
        withResultSaving fileName getResponse
    pure $ catMaybes $ parseMaybe parseJSON <$> resValue
    where
        reloadHandler ma = (`handle` ma) $ \e -> case fromException e of
            Just UserInterrupt -> throwM UserInterrupt
            _ -> do
                ct <- liftIO getCurrentTime
                mkLog ( show ct <> "\n" <> show e <> "\n(Handled)")
                liftIO (threadDelay 5_000_000)
                reloadHandler ma

withResultSaving :: (MonadIO m, FromJSON a, ToJSON a) => FilePath -> m a -> m a
withResultSaving fp action = 
    liftIO (try @_ @SomeException $ either error id <$> eitherDecodeFileStrict fp) 
        >>= either doAction pure
    where
        doAction _ = do
            res <- action
            _   <- liftIO $ writeFileJSON fp res
            pure res

divideTimeIntoIntervals :: Slot -> Slot -> Slot -> [(Slot, Slot)]
divideTimeIntoIntervals from to delta = do
    let xs = [from, from + delta .. to]
    zip (init xs) (subtract 1 <$> tail xs) <> [(last xs, to)]
