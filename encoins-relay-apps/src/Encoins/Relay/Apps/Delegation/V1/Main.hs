{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Apps.Delegation.V1.Main where

import           Cardano.Api                            (NetworkId (..), writeFileJSON)
import           Cardano.Node.Emulator                  (SlotConfig)
import           Cardano.Server.Config                  (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Utils.Logger            ((.<))
import           Cardano.Server.Utils.Wait              (waitTime)
import           Control.Concurrent.Async               (async, wait)
import           Control.Monad                          (MonadPlus (..), forM, forever, guard, unless, void)
import           Control.Monad.Trans.Class              (MonadTrans (..))
import           Control.Monad.Trans.Maybe              (MaybeT (..))
import           Data.Aeson.Types                       (parseEither)
import           Data.Bifunctor                         (Bifunctor (..))
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (catMaybes, fromMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import qualified Data.Time                              as Time
import           Encoins.Relay.Apps.Delegation.Internal (DelegConfig (..), Delegation (..), concatIpsWithBalances, isValidIp,
                                                         removeDuplicates)
import           Encoins.Relay.Apps.Internal            (formatTime, loadMostRecentFile, progressBarStyle, withResultSaving)
import qualified Encoins.Relay.Apps.Internal            as Internal
import           Encoins.Relay.Server.Config            (EncoinsRelayConfig (..))
import           Ledger                                 (Address (..), Datum (..), DatumHash, Slot, StakingCredential, TxId (..),
                                                         TxOutRef (..))
import           Plutus.V1.Ledger.Api                   (Credential (PubKeyCredential), StakingCredential (..))
import           Plutus.V1.Ledger.Value                 (TokenName)
import           Plutus.V2.Ledger.Api                   (CurrencySymbol)
import qualified PlutusAppsExtra.Api.Kupo               as Kupo
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo     as Kupo
import           PlutusAppsExtra.Utils.Address          (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo             (KupoResponse (..), SlotWithHeaderHash (swhhSlot))
import           PlutusAppsExtra.Utils.Time             (parseSlotConfig, utcToSlot)
import           PlutusAppsExtra.Utils.Tx               (txIsSignedByKey)
import           PlutusTx                               (FromData (..))
import           PlutusTx.Builtins                      (BuiltinByteString, decodeUtf8, fromBuiltin)
import           System.Directory                       (createDirectoryIfMissing)
import           System.ProgressBar                     (Progress (..), ProgressBar, incProgress, newProgressBar)

main :: FilePath -> FilePath -> IO ()
main configFp delegConfigFp = do
    config      <- decodeOrErrorFromFile configFp
    relayConfig <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
    delegConfig <- decodeOrErrorFromFile @DelegConfig delegConfigFp
    slotConfig  <- either error id . parseEither parseSlotConfig <$> decodeOrErrorFromFile (cSlotConfigFile config)
    let delegFolder = cDelegationFolder delegConfig
        handle = mkDelegationHandle config slotConfig
    createDirectoryIfMissing True delegFolder
    forever $ do
        ct                         <- Time.addUTCTime (- 300) <$> Time.getCurrentTime
        delay                      <- async $ waitTime 300
        (mbTime, mbPastDelegators) <- maybe (Nothing, Nothing) (bimap Just Just) <$> loadPastProgress delegFolder
        let start = maybe (delegationStart $ cNetworkId config) (utcToSlot slotConfig) mbTime
        delegators <- findDelegators delegFolder handle start (utcToSlot slotConfig ct)
        let res = filter (`notElem` delegators) (fromMaybe [] mbPastDelegators) <> delegators
        print res
        void $ writeFileJSON (delegFolder <> "/delegators_" <> formatTime ct <> ".json") res
        ipsWithBalances <- concatIpsWithBalances <$> mapM (getIpWithBalance handle relayConfig) res
        writeResultFile delegFolder ct ipsWithBalances
        wait delay
    where
        getIpWithBalance h EncoinsRelayConfig{..} Delegation{..} = do
            let stakeKey = StakingHash $ PubKeyCredential delegStakeKey
            balance <- dhGetTokenBalance h cDelegationCurrencySymbol cDelegationTokenName stakeKey
            pure (delegIp, balance)
        delegationStart = \case
            Mainnet -> 106866682
            _       -> 0

findDelegators :: forall m. Monad m => FilePath -> DelegationHandle m -> Slot -> Slot -> m [Delegation]
findDelegators delegationFolder DelegationHandle{..} slotFrom slotTo = do
        responses <- dhGetResponses (Just slotFrom) (Just slotTo)
        pb <- dhNewProgressBar "Getting delegated txs" $ length responses
        fmap (removeDuplicates . catMaybes) $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            lift $ dhIncProgress pb 1
            dhWithResultSaving (mconcat [delegationFolder, "/deleg_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getDelegationFromResponse KupoResponse{..}
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

loadPastProgress :: FilePath -> IO (Maybe (Time.UTCTime, [Delegation]))
loadPastProgress delegFolder = loadMostRecentFile delegFolder "delegators_"

writeResultFile :: FilePath -> Time.UTCTime -> Map Text Integer -> IO ()
writeResultFile delegFolder ct ipsWithBalances =
    let result = Map.map (T.pack . show) ipsWithBalances
    in  void $ writeFileJSON (delegFolder <> "/result_" <> formatTime ct <> ".json") result

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
    { dhGetResponses     = getResponsesIO cNetworkId slotConfig
    , dhGetDatumByHash   = Kupo.getDatumByHash
    , dhGetTokenBalance  = getTokenBalanceIO
    , dhCheckTxSignature = txIsSignedByKey
    , dhMkLog            = T.putStrLn
    , dhWithResultSaving = withResultSaving
    , dhNewProgressBar   = \m p -> newProgressBar (progressBarStyle m) 10 (Progress 0 p ())
    , dhIncProgress      = incProgress
    }

getResponsesIO :: NetworkId -> SlotConfig -> Maybe Slot -> Maybe Slot -> IO [KupoResponse]
getResponsesIO networkId slotConfig from to = do
    to' <- maybe (utcToSlot slotConfig <$> Time.getCurrentTime) pure to
    putStrLn $ "getting responses from " <> show (fromMaybe 0 from) <> " to " <> show to'
    Internal.getResponsesIO networkId (fromMaybe 0 from) to' 3600

getTokenBalanceIO :: CurrencySymbol -> TokenName -> StakingCredential -> IO Integer
getTokenBalanceIO cs tokenName = Kupo.getTokenBalanceToSlot cs tokenName Nothing