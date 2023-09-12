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
import           Cardano.Server.Internal            (loadSlotConfig)
import           Cardano.Server.Utils.Logger        ((.<))
import           Control.Arrow                      ((>>>))
import           Control.Monad                      (MonadPlus (..), forM, forever, join, unless, (>=>), guard)
import           Control.Monad.Trans.Class          (MonadTrans (..))
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (eitherDecodeFileStrict)
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
import           Encoins.Relay.Apps.Internal        (getResponsesIO, withResultSaving)
import           Encoins.Relay.Server.Config        (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Delegation    (Delegation (..))
import           Ledger                             (Address (..), CurrencySymbol, Datum (..), DatumHash, PubKeyHash (PubKeyHash),
                                                     Slot, StakingCredential, TokenName, TxId, TxOutRef (..))
import           Network.URI                        (isIPv4address, isURI)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (swhhSlot))
import           PlutusAppsExtra.Utils.Time         (utcToSlot)
import           PlutusAppsExtra.Utils.Tx           (txIsSignedByKey)
import           PlutusTx                           (FromData (..))
import           PlutusTx.Builtins                  (BuiltinByteString, decodeUtf8, fromBuiltin)
import           System.Directory                   (createDirectoryIfMissing, getCurrentDirectory, listDirectory,
                                                     setCurrentDirectory)
import           Text.Read                          (readMaybe)

main :: FilePath -> IO ()
main configFp = do
        config      <- decodeOrErrorFromFile configFp
        relayConfig <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
        slotConfig  <- loadSlotConfig $ cSlotConfigFile config
        let handle = mkDelegationHandle config slotConfig
        createDirectoryIfMissing True $ cDelegationFolder relayConfig
        forever $ do
            ct                         <- Time.getCurrentTime
            (mbPastDelegators, mbTime) <- getPastDelegators $ cDelegationFolder relayConfig
            let start = maybe (cDelegationStart relayConfig) (utcToSlot slotConfig) mbTime
            delegators <- findDelegators (cDelegationFolder relayConfig) handle start
            let res = filter (`notElem` delegators) (fromMaybe [] mbPastDelegators) <> delegators
            print res
            writeFileJSON (cDelegationFolder relayConfig <> "/delegators_" <> show ct <> ".json") res
    where
        getPastDelegators delegationFolder = do
            setCurrentDirectory delegationFolder
            files <- getCurrentDirectory >>= listDirectory
            let time = listToMaybe . reverse . sort $ mapMaybe (stripPrefix "delegators_" >=> takeWhile (/= '.') >>> readMaybe @Time.UTCTime) files
                fp = (\t -> "delegators_" <> t <> ".json") . show <$> time
            delegators <- fmap join $ sequence $ fmap eitherToMaybe . eitherDecodeFileStrict <$> fp
            setCurrentDirectory ".."
            pure (delegators, time)

findDelegators :: forall m. Monad m => FilePath -> DelegationHandle m -> Slot -> m [Delegation]
findDelegators delegationFolder DelegationHandle{..} slotFrom = do

        dhMkLog "Getting reponses..."
        !ixResponses <- zip [(1 :: Int)..] <$> dhGetResponses (Just slotFrom) Nothing
        let l = length ixResponses

        dhMkLog "Getting delegated txs..."
        fmap (removeDuplicates . catMaybes) $ forM ixResponses $ \(ix, KupoResponse{..}) -> runMaybeT $ do
            lift $ dhMkLog $ T.pack $ show ix <> "/" <> show l
            dhWithResultSaving (mconcat [delegationFolder, "/deleg_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getDelegationFromResponse KupoResponse{..}

    where
        -- Token balance validation occurs at rewards distribution
        getDelegationFromResponse :: KupoResponse -> MaybeT m Delegation
        getDelegationFromResponse KupoResponse{..} = do
            dh                             <- MaybeT $ pure krDatumHash
            (Datum dat)                    <- MaybeT $ dhGetDatumByHash dh
            ("ENCS Delegation", ipAddrBbs) <- MaybeT $ pure $ fromBuiltinData @(BuiltinByteString, BuiltinByteString) dat
            stakeKey                       <- MaybeT $ pure $ getStakeKey krAddress
            guard =<< lift (dhCheckTxSignature krTxId krAddress)
            let ipAddr = fromBuiltin $ decodeUtf8 ipAddrBbs
            unless (isValidIp ipAddr) $ lift (dhMkLog $ "Invalid ip: " .< ipAddr) >> mzero
            pure $ Delegation (addressCredential krAddress) stakeKey (TxOutRef krTxId krOutputIndex) (swhhSlot krCreatedAt) ipAddr

        removeDuplicates = fmap (NonEmpty.head . NonEmpty.sortBy (compare `on` Down . delegCreated))
                         . NonEmpty.groupBy ((==) `on` delegStakeKey)
                         . sortBy (compare `on` delegStakeKey)

isValidIp :: Text -> Bool
isValidIp txt = or $ [isURI, isIPv4address] <&> ($ T.unpack txt)

data DelegationHandle m = DelegationHandle
    { dhGetResponses     :: Maybe Slot -> Maybe Slot -> m [KupoResponse]
    , dhGetDatumByHash   :: DatumHash -> m (Maybe Datum)
    , dhGetTokenBalance  :: CurrencySymbol -> TokenName -> StakingCredential -> m Integer
    , dhCheckTxSignature :: TxId -> Address -> m Bool
    , dhMkLog            :: Text -> m ()
    , dhWithResultSaving :: FilePath -> MaybeT m Delegation -> MaybeT m Delegation
    }

mkDelegationHandle :: Config -> SlotConfig -> DelegationHandle IO
mkDelegationHandle Config{..} slotConfig = DelegationHandle
    { dhGetResponses     = getResponesesIO cNetworkId slotConfig
    , dhGetDatumByHash   = Kupo.getDatumByHash
    , dhGetTokenBalance  = getTokenBalanceIO
    , dhCheckTxSignature = checkTxSignatureIO
    , dhMkLog            = T.putStrLn
    , dhWithResultSaving = withResultSaving
    }

getResponesesIO :: NetworkId -> SlotConfig -> Maybe Slot -> Maybe Slot -> IO [KupoResponse]
getResponesesIO networkId slotConfig from to = do
    to' <- maybe (utcToSlot slotConfig <$> Time.getCurrentTime) pure to
    putStrLn $ "getting responses from " <> show (fromMaybe 0 from) <> " to " <> show to'
    getResponsesIO networkId (fromMaybe 0 from) to' 3600

getTokenBalanceIO :: CurrencySymbol -> TokenName -> StakingCredential -> IO Integer
getTokenBalanceIO cs tokenName = Kupo.getTokenBalanceToSlot cs tokenName Nothing

checkTxSignatureIO :: TxId -> Address -> IO Bool
checkTxSignatureIO txId addr = case getStakeKey addr of
    Just (PubKeyHash pkh) -> txIsSignedByKey txId pkh
    _ -> pure False