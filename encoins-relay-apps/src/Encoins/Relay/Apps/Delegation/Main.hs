{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Apps.Delegation.Main where

import           Cardano.Api                        (NetworkId, writeFileJSON)
import           Cardano.Node.Emulator              (SlotConfig)
import           Cardano.Server.Config              (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Utils.Logger        ((.<))
import           Cardano.Server.Utils.Wait          (waitTime)
import           Control.Arrow                      ((>>>))
import           Control.Concurrent.Async           (async, wait)
import           Control.Monad                      (MonadPlus (..), forM, forever, guard, join, unless, void, (>=>))
import           Control.Monad.Trans.Class          (MonadTrans (..))
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (eitherDecodeFileStrict)
import           Data.Aeson.Types                   (parseEither)
import           Data.Either.Extra                  (eitherToMaybe)
import           Data.Function                      (on)
import           Data.Functor                       ((<&>))
import           Data.List                          (sort, sortBy, stripPrefix)
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord                           (Down (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import qualified Data.Time                          as Time
import           Encoins.Relay.Apps.Internal        (getResponsesIO, progressBarStyle, withResultSaving)
import           Encoins.Relay.Server.Config        (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Delegation    (Delegation (..))
import           Ledger                             (Address (..), Datum (..), DatumHash, Slot, StakingCredential, TxId (..),
                                                     TxOutRef (..))
import           Network.URI                        (isIPv4address, isURI)
import           Plutus.V1.Ledger.Value             (TokenName)
import           Plutus.V2.Ledger.Api               (CurrencySymbol)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (swhhSlot))
import           PlutusAppsExtra.Utils.Time         (parseSlotConfig, utcToSlot)
import           PlutusAppsExtra.Utils.Tx           (txIsSignedByKey)
import           PlutusTx                           (FromData (..))
import           PlutusTx.Builtins                  (BuiltinByteString, decodeUtf8, fromBuiltin)
import           System.Directory                   (createDirectoryIfMissing, listDirectory)
import           System.ProgressBar                 (Progress (..), ProgressBar, incProgress, newProgressBar)


main :: FilePath -> IO ()
main configFp = do
        config      <- decodeOrErrorFromFile configFp
        relayConfig <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
        slotConfig  <- either error id . parseEither parseSlotConfig <$> decodeOrErrorFromFile (cSlotConfigFile config)
        let handle = mkDelegationHandle config slotConfig
        createDirectoryIfMissing True $ cDelegationFolder relayConfig
        forever $ do
            ct                         <- Time.addUTCTime (- 1800) <$> Time.getCurrentTime
            delay                      <- async $ waitTime 300
            (mbPastDelegators, mbTime) <- getPastDelegators $ cDelegationFolder relayConfig
            let start = maybe (cDelegationStart relayConfig) (utcToSlot slotConfig) mbTime
            delegators <- findDelegators (cDelegationFolder relayConfig) handle start
            let res = filter (`notElem` delegators) (fromMaybe [] mbPastDelegators) <> delegators
            print res
            void $ writeFileJSON (cDelegationFolder relayConfig <> "/delegators_" <> formatTime ct <> ".json") res
            wait delay
    where
        getPastDelegators delegationFolder = do
            files <- listDirectory delegationFolder
            let time = listToMaybe . reverse . sort $ mapMaybe (stripPrefix "delegators_" >=> takeWhile (/= '.') >>> readTime) files
                fp = (\t -> delegationFolder <> "/delegators_" <> t <> ".json") . formatTime <$> time
            delegators <- fmap join $ sequence $ fmap eitherToMaybe . eitherDecodeFileStrict <$> fp
            pure (delegators, time)
        formatTime   = Time.formatTime Time.defaultTimeLocale formatString
        readTime     = Time.parseTimeM True Time.defaultTimeLocale formatString
        formatString = "%d-%b-%YT%H:%M:%S"

findDelegators :: forall m. Monad m => FilePath -> DelegationHandle m -> Slot -> m [Delegation]
findDelegators delegationFolder DelegationHandle{..} slotFrom = do
        !responses <- dhGetResponses (Just slotFrom) Nothing
        pb <- dhNewProgressBar "Getting delegated txs" $ length responses
        fmap (removeDuplicates . catMaybes) $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            d <- dhWithResultSaving (mconcat [delegationFolder, "/deleg_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getDelegationFromResponse KupoResponse{..}
            lift $ dhIncProgress pb 1
            pure d
    where
        -- Token balance validation occurs at rewards distribution
        getDelegationFromResponse :: KupoResponse -> MaybeT m Delegation
        getDelegationFromResponse KupoResponse{..} = do
            dh                                    <- MaybeT $ pure krDatumHash
            (Datum dat)                           <- MaybeT $ dhGetDatumByHash dh
            ["ENCOINS", "Delegate", skBbs, ipBbs] <- MaybeT $ pure $ fromBuiltinData dat
            lift $ dhMkLog $ T.pack $  "Tx found:" <> show krTxId
            stakeKey                              <- MaybeT $ pure $ getStakeKey krAddress
            lift $ dhMkLog $ T.pack $ show stakeKey
            guard =<< lift (dhCheckTxSignature krTxId skBbs)
            lift $ dhMkLog "signature is ok"
            let ipAddr = fromBuiltin $ decodeUtf8 ipBbs
            unless (isValidIp ipAddr) $ do
                lift $ dhMkLog $ "Invalid ip: " .< ipAddr
                mzero
            pure $ Delegation (addressCredential krAddress) stakeKey (TxOutRef krTxId krOutputIndex) (swhhSlot krCreatedAt) ipAddr

        removeDuplicates = fmap (NonEmpty.head . NonEmpty.sortBy (compare `on` Down . delegCreated))
                         . NonEmpty.groupBy ((==) `on` delegStakeKey)
                         . sortBy (compare `on` delegStakeKey)

isValidIp :: Text -> Bool
isValidIp txt = or $ [isSimpleURI, isURI, isIPv4address] <&> ($ T.unpack txt)
    where
        isSimpleURI "" = False
        isSimpleURI _  = case T.splitOn "." txt of [_, _] -> True; _ -> False

data DelegationHandle m = DelegationHandle
    { dhGetResponses     :: Maybe Slot -> Maybe Slot -> m [KupoResponse]
    , dhGetDatumByHash   :: DatumHash -> m (Maybe Datum)
    , dhGetTokenBalance  :: CurrencySymbol -> TokenName -> StakingCredential -> m Integer
    , dhCheckTxSignature :: TxId -> BuiltinByteString -> m Bool
    , dhMkLog            :: Text -> m ()
    , dhWithResultSaving :: FilePath -> MaybeT m Delegation -> MaybeT m Delegation
    -- Progress bar
    , dhNewProgressBar   :: Text -> Int -> m (ProgressBar ())
    , dhIncProgress      :: ProgressBar () -> Int -> m ()
    }

mkDelegationHandle :: Config -> SlotConfig -> DelegationHandle IO
mkDelegationHandle Config{..} slotConfig = DelegationHandle
    { dhGetResponses     = getResponesesIO cNetworkId slotConfig
    , dhGetDatumByHash   = Kupo.getDatumByHash
    , dhGetTokenBalance  = getTokenBalanceIO
    , dhCheckTxSignature = txIsSignedByKey
    , dhMkLog            = T.putStrLn
    , dhWithResultSaving = withResultSaving
    , dhNewProgressBar   = \m p -> newProgressBar (progressBarStyle m) 10 (Progress 0 p ())
    , dhIncProgress      = incProgress
    }

getResponesesIO :: NetworkId -> SlotConfig -> Maybe Slot -> Maybe Slot -> IO [KupoResponse]
getResponesesIO networkId slotConfig from to = do
    to' <- maybe (utcToSlot slotConfig <$> Time.getCurrentTime) pure to
    putStrLn $ "getting responses from " <> show (fromMaybe 0 from) <> " to " <> show to'
    getResponsesIO networkId (fromMaybe 0 from) to' 3600

getTokenBalanceIO :: CurrencySymbol -> TokenName -> StakingCredential -> IO Integer
getTokenBalanceIO cs tokenName = Kupo.getTokenBalanceToSlot cs tokenName Nothing