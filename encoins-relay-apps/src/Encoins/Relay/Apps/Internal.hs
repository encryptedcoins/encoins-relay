{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Apps.Internal where

import           Cardano.Api                        (NetworkId, writeFileJSON)
import           Control.Arrow                      (Arrow ((&&&)), (<<<), (>>>))
import           Control.Concurrent                 (threadDelay)
import           Control.Exception                  (AsyncException (UserInterrupt), Exception (..), SomeException)
import           Control.Monad                      (forM, join, (>=>))
import           Control.Monad.Catch                (MonadCatch, MonadThrow (throwM), handle, try)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Data.Aeson                         (FromJSON (parseJSON), ToJSON, eitherDecodeFileStrict)
import           Data.Aeson.Types                   (parseMaybe)
import           Data.Default                       (def)
import           Data.Either.Extra                  (eitherToMaybe)
import           Data.List                          (isPrefixOf, sort, stripPrefix)
import           Data.Maybe                         (catMaybes, listToMaybe, mapMaybe)
import           Data.Text                          (Text)
import qualified Data.Text.Lazy                     as TL
import           Data.Time                          (getCurrentTime)
import qualified Data.Time                          as Time
import           Ledger                             (Slot (getSlot))
import           Plutus.V2.Ledger.Api               (CurrencySymbol, TokenName)
import           PlutusAppsExtra.Api.Kupo           (CreatedOrSpent (..), KupoRequest (..), SpentOrUnspent (..), getKupoResponse)
import           PlutusAppsExtra.IO.ChainIndex.Kupo ()
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), kupoResponseToJSON)
import           System.Directory                   (createDirectoryIfMissing, listDirectory, removeFile)
import           System.FilePath                    ((</>))
import           System.ProgressBar                 (Progress (..), ProgressBar, ProgressBarWidth (..), Style (..), defStyle,
                                                     exact, incProgress, msg)
import qualified System.ProgressBar                 as PB

encoinsTokenName :: TokenName
encoinsTokenName = "ENCS"

-- Mainnet only
encoinsCS :: CurrencySymbol
encoinsCS = "9abf0afd2f236a19f2842d502d0450cbcd9c79f123a9708f96fd9b96"

getResponsesIO :: (MonadIO m, MonadCatch m) => NetworkId -> Slot -> Slot -> Slot -> m [KupoResponse]
getResponsesIO networkId slotFrom slotTo slotDelta = do
    liftIO $ createDirectoryIfMissing True "savedResponses"
    pb <- liftIO $ newProgressBar "Getting reponses" (length intervals)
    resValue <- fmap concat $ forM intervals $ \(from, to) -> reloadHandler $ do
        let fileName = "response" <> show (getSlot from) <> "_"  <> show (getSlot to) <> ".json"
            req :: KupoRequest 'SUSpent 'CSCreated 'CSCreated
            req = def{reqCreatedOrSpentAfter = Just from, reqCreatedOrSpentBefore = Just to}
        r <- withResultSaving ("savedResponses/" <> fileName) $ liftIO $
            fmap (kupoResponseToJSON networkId) <$> getKupoResponse req
        liftIO $ incProgress pb 1
        pure r
    pure $ catMaybes $ parseMaybe parseJSON <$> resValue
    where
        intervals = divideTimeIntoIntervals slotFrom slotTo slotDelta
        mkLog = liftIO . putStrLn
        reloadHandler ma = (`handle` ma) $ \e -> case fromException e of
            Just UserInterrupt -> throwM UserInterrupt
            _ -> do
                ct <- liftIO getCurrentTime
                mkLog (show ct <> "\n" <> show e <> "\n(Handled)")
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

-- Divide time into intervals such that each interval except the first is a multiple of delta.
divideTimeIntoIntervals :: Slot -> Slot -> Slot -> [(Slot, Slot)]
divideTimeIntoIntervals from to delta
    | from > to         = []
    | to - from < delta = [(from, to)]
    | from /= from'     = (from, from' - 1) : divideTimeIntoIntervals from' to delta
    | otherwise         = zip (init xs) (subtract 1 <$> tail xs) <> [(last xs, to)]
    where
        -- First delta multiplier
        from' = head [x | x <- [from ..], x `mod` delta == 0]
        xs = [from, from + delta .. to]

defaultSlotConfigFilePath :: FilePath
defaultSlotConfigFilePath = "../plutus-chain-index/plutus-chain-index-config.json"

progressBarStyle :: Text -> Style s
progressBarStyle m = defStyle
    { stylePrefix  = msg $ TL.fromStrict m
    , styleWidth   = ConstantWidth 100
    , stylePostfix = exact
    }

newProgressBar :: MonadIO m => Text -> Int -> m (ProgressBar ())
newProgressBar name n = liftIO $ PB.newProgressBar (progressBarStyle name) 10 (Progress 0 n ())

formatString :: String
formatString = "%d-%b-%YT%H:%M:%S"

formatTime :: Time.UTCTime -> String
formatTime = Time.formatTime Time.defaultTimeLocale formatString

readTime :: MonadFail m => String -> m Time.UTCTime
readTime = Time.parseTimeM True Time.defaultTimeLocale formatString

loadMostRecentFile :: FromJSON a => FilePath -> String -> IO (Maybe (Time.UTCTime, a))
loadMostRecentFile dir prefix = do
    files <- listDirectory dir
    let time = listToMaybe . reverse . sort $ mapMaybe (stripPrefix prefix >=> takeWhile (/= '.') >>> readTime) files
        fp = (\t -> dir </> prefix <> t <> ".json") . formatTime <$> time
    res <- fmap join $ sequence $ fmap eitherToMaybe . eitherDecodeFileStrict <$> fp
    pure $ (,) <$> time <*> res

janitorFiles :: MonadIO m => FilePath -> String -> m ()
janitorFiles  dir prefix = liftIO $ do
        files <- listDirectory dir
        let mbLastTime = listToMaybe . reverse . sort $ mapMaybe (stripPrefix prefix >=> takeWhile (/= '.') >>> readTime) files
            mbLastFile = (\t -> dir </> prefix <> t <> ".json") . formatTime <$> mbLastTime
            toRemove = (`filterFiles` map (dir </>) files) <$> mbLastFile
        sequence_ $ mapM_ removeFile <$> toRemove
    where
        filterFiles lastFile = filter $ uncurry (&&) <<< (/= lastFile) &&& isPrefixOf (dir </> prefix)