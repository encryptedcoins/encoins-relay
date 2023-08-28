{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Apps.Delegation.Main where

import           Cardano.Api                          (writeFileJSON)
import           Cardano.Server.Utils.Logger          ((.<))
import           Control.Arrow                        ((>>>))
import           Control.Monad                        (MonadPlus (mzero), forM, forever, guard, join, unless, (>=>))
import           Control.Monad.Trans.Class            (MonadTrans (..))
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Aeson                           (FromJSON (..), ToJSON (..), eitherDecodeFileStrict)
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Either.Extra                    (eitherToMaybe)
import           Data.Function                        (on)
import           Data.Functor                         ((<&>))
import           Data.List                            (sort, sortBy, stripPrefix)
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Maybe                           (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord                             (Down (..))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Time                            as Time
import           Encoins.Relay.Apps.Delegation.Config (DelegationConfig (..), getConfig)
import           Encoins.Relay.Apps.Internal          (getResponsesIO, withResultSaving)
import           GHC.Generics                         (Generic)
import           Ledger                               (Address, CurrencySymbol, Datum (..), DatumHash, PubKeyHash (PubKeyHash),
                                                       Slot, StakingCredential, TokenName, TxId, TxOutRef (..))
import           Network.URI                          (isIPv4address, isURI)
import           Plutus.V1.Ledger.Credential          (Credential (PubKeyCredential), StakingCredential (StakingHash))
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo   as Kupo
import           PlutusAppsExtra.Utils.Address        (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo           (KupoResponse (..), SlotWithHeaderHash (swhhSlot))
import           PlutusAppsExtra.Utils.Time           (utcToSlot)
import           PlutusAppsExtra.Utils.Tx             (txIsSignedByKey)
import           PlutusTx                             (FromData (..))
import           PlutusTx.Builtins                    (decodeUtf8, fromBuiltin)
import           System.Directory                     (getCurrentDirectory, listDirectory)
import           Text.Read                            (readMaybe)

main :: FilePath -> IO ()
main configFp = forever $ do
        ct <- Time.getCurrentTime
        conf <- getConfig configFp
        (pastDelegators, time) <- getPastDelegators
        let start = maybe (dcDelegationStart conf) (utcToSlot $ dcSlotConfig conf) time
            handle = mkDelegationHandle conf
        delegators <- findDelegators (conf{dcDelegationStart = start}) handle
        let res = filter (`notElem` delegators) (fromMaybe [] pastDelegators) <> delegators
        print res
        writeFileJSON ("delegators_" <> show ct <> ".json") res
    where
        getPastDelegators = do
            files <- getCurrentDirectory >>= listDirectory
            let time = listToMaybe . reverse . sort $ mapMaybe (stripPrefix "delegators_" >=> takeWhile (/= '.') >>> readMaybe @Time.UTCTime) files
                fp = (\t -> "delegators_" <> t <> ".json") . show <$> time
            delegators <- fmap join $ sequence $ fmap eitherToMaybe . eitherDecodeFileStrict <$> fp
            pure (delegators, time)

findDelegators :: forall m. Monad m => DelegationConfig -> DelegationHandle m -> m [Text]
findDelegators DelegationConfig{..} DelegationHandle{..} = do
        let folderName = "delegation_files"

        dhMkLog "Getting reponses..."
        !responses <- dhGetResponses

        dhMkLog "Getting delegation txs..."
        delegations <- fmap (removeDuplicates . catMaybes) $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            dhWithResultSaving (mconcat [folderName, "/deleg_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getDelegationFromResponse KupoResponse{..}

        dhMkLog "Getting delegators..."
        let l = length delegations
        fmap catMaybes $ forM (zip [1 :: Int ..] delegations) $ \(i, d) -> runMaybeT $ do
            lift $ dhMkLog $ T.pack $ show i <> "/" <> show l
            getIpFromDelegation d

    where
        getDelegationFromResponse :: KupoResponse -> MaybeT m Delegation
        getDelegationFromResponse KupoResponse{..} = do
            dh                          <- MaybeT $ pure krDatumHash
            (Datum dat)                 <- MaybeT $ dhGetDatumByHash dh
            ("ENCS Delegation", ipAddr) <- MaybeT $ pure $ join bimap (fromBuiltin . decodeUtf8) <$> fromBuiltinData dat
            stakeKey                    <- MaybeT $ pure $ getStakeKey krAddress
            guard =<< lift (dhCheckTxSignature krTxId krAddress)
            pure $ Delegation stakeKey (TxOutRef krTxId krOutputIndex) (swhhSlot krCreatedAt) ipAddr

        getIpFromDelegation :: Delegation -> MaybeT m Text
        getIpFromDelegation Delegation{..} = do
            balance <- lift $ dhGetTokenBalance dcCs dcTokenName (StakingHash $ PubKeyCredential delegPkh)
            guard $ balance >= dcMinTokenAmount
            unless (isValidIp delegIp) $ lift (dhMkLog $ "Invalid ip: " .< delegIp) >> mzero
            pure delegIp

        removeDuplicates = fmap (NonEmpty.head . NonEmpty.sortBy (compare `on` Down . delegCreated))
                         . NonEmpty.groupBy ((==) `on` delegPkh)
                         . sortBy (compare `on` delegPkh)

isValidIp :: Text -> Bool
isValidIp txt = or $ [isURI, isIPv4address] <&> ($ T.unpack txt)

data Delegation = Delegation
    { delegPkh      :: PubKeyHash
    , delegTxOutRef :: TxOutRef
    , delegCreated  :: Slot
    , delegIp       :: Text
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DelegationHandle m = DelegationHandle
    { dhGetResponses     :: m [KupoResponse]
    , dhGetDatumByHash   :: DatumHash -> m (Maybe Datum)
    , dhGetTokenBalance  :: CurrencySymbol -> TokenName -> StakingCredential -> m Integer
    , dhCheckTxSignature :: TxId -> Address -> m Bool
    , dhMkLog            :: Text -> m ()
    , dhWithResultSaving :: FilePath -> MaybeT m Delegation -> MaybeT m Delegation
    }

mkDelegationHandle :: DelegationConfig -> DelegationHandle IO
mkDelegationHandle DelegationConfig{..} = DelegationHandle
        (getResponses dcSlotConfig)
        Kupo.getDatumByHash
        getTokenBalanceIO
        checkTxSignatureIO
        T.putStrLn
        withResultSaving
    where
        getResponses slotConfig = do
            slotTo <- utcToSlot slotConfig <$> Time.getCurrentTime
            getResponsesIO dcNetworkId dcDelegationStart slotTo 3600

getTokenBalanceIO :: CurrencySymbol -> TokenName -> StakingCredential -> IO Integer
getTokenBalanceIO cs tokenName = Kupo.getTokenBalanceToSlot cs tokenName Nothing

checkTxSignatureIO :: TxId -> Address -> IO Bool
checkTxSignatureIO txId addr = case getStakeKey addr of
    Just (PubKeyHash pkh) -> txIsSignedByKey txId pkh
    _ -> pure False