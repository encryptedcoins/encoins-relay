{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Delegation where

import           Cardano.Api                        (writeFileJSON)
import           Cardano.Server.Config              (decodeOrErrorFromFile)
import           Control.Arrow                      ((>>>))
import           Control.Monad                      (forM, guard, join, void, (>=>))
import           Control.Monad.Trans.Class          (MonadTrans (..))
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (FromJSON, ToJSON, eitherDecodeFileStrict)
import           Data.Bifunctor                     (Bifunctor (..))
import           Data.Default                       (Default (..))
import           Data.Either.Extra                  (eitherToMaybe)
import           Data.Function                      (on)
import           Data.List                          (sort, sortBy, stripPrefix)
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord                           (Down (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import qualified Data.Time                          as Time
import           Encoins.Relay.Poll.Config          (encoinsTokenName, utcToSlot)
import           GHC.Generics                       (Generic)
import           Ledger                             (Address, CurrencySymbol, Datum (..), DatumHash, NetworkId,
                                                     PubKeyHash (PubKeyHash), Slot, StakingCredential, TokenName, TxId,
                                                     TxOutRef (..))
import           Plutus.V1.Ledger.Credential        (Credential (PubKeyCredential), StakingCredential (StakingHash))
import           PlutusAppsExtra.IO.ChainIndex.Kupo (CreatedOrSpent (..), KupoRequest, SpentOrUnspent (..))
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (swhhSlot))
import           PlutusAppsExtra.Utils.Tx           (txIsSignedByKey)
import           PlutusTx                           (FromData (..))
import           PlutusTx.Builtins                  (decodeUtf8, fromBuiltin)
import           System.Directory                   (getCurrentDirectory, listDirectory)
import           Text.Read                          (readMaybe)

main :: FilePath -> IO ()
main configFp = void $ do
        conf <- decodeOrErrorFromFile configFp
        (pastDelegators, time) <- getPastDelegators
        let start = maybe (dcDelegationStart conf) utcToSlot time
        delegators <- findDelegators (conf{dcDelegationStart = start}) (mkDelegationHandle conf)
        let res = filter (`notElem` delegators) (fromMaybe [] pastDelegators) <> delegators
        print res
        ct <- Time.getCurrentTime
        writeFileJSON ("delegators_" <> show ct <> ".json") res
    where
        getPastDelegators = do
            files <- getCurrentDirectory >>= listDirectory
            let time = listToMaybe . reverse . sort $ mapMaybe (stripPrefix "delegators_" >=> takeWhile (/= '.') >>> readMaybe @Time.UTCTime) files
                fp = (<> ".json") . ("delegators_" <>) . show <$> time
            delegators <- fmap join $ sequence $ fmap eitherToMaybe . eitherDecodeFileStrict <$> fp
            pure (delegators, time)

findDelegators :: forall m. Monad m => DelegationConfig -> DelegationHandle m -> m [Text]
findDelegators DelegationConfig{..} DelegationHandle{..} = do
        let folderName = "delegation_files"

        mkLog "Getting reponses..."
        !responses <- getResponses

        mkLog "Getting delegation txs..."
        delegations <- fmap (removeDuplicates . catMaybes) $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            withResultSaving (mconcat [folderName, "/deleg_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getDelegationFromResponse KupoResponse{..}

        mkLog "Getting delegators..."
        let l = length delegations
        fmap catMaybes $ forM (zip [1 :: Int ..] delegations) $ \(i, d) -> runMaybeT $ do
            lift $ mkLog $ T.pack $ show i <> "/" <> show l
            getIpFromDelegation d

    where
        getDelegationFromResponse :: KupoResponse -> MaybeT m Delegation
        getDelegationFromResponse KupoResponse{..} = do
            dh                          <- MaybeT $ pure krDatumHash
            (Datum dat)                 <- MaybeT $ getDatumByHash dh
            ("ENCS Delegation", ipAddr) <- MaybeT $ pure $ join bimap (fromBuiltin . decodeUtf8) <$> fromBuiltinData dat
            stakeKey                    <- MaybeT $ pure $ getStakeKey krAddress
            guard =<< lift (checkTxSignature krTxId krAddress)
            pure $ Delegation stakeKey (TxOutRef krTxId krOutputIndex) (swhhSlot krCreatedAt) ipAddr

        getIpFromDelegation :: Delegation -> MaybeT m Text
        getIpFromDelegation Delegation{..} = do
            balance <- lift $ getTokenBalance dcCurrencySymbol encoinsTokenName (StakingHash $ PubKeyCredential delegPkh)
            guard $ balance >= dcMinTokenAmount
            pure delegIp

        removeDuplicates = fmap (NonEmpty.head . NonEmpty.sortBy (compare `on` Down . delegCreated))
                         . NonEmpty.groupBy ((==) `on` delegPkh)
                         . sortBy (compare `on` delegPkh)

data Delegation = Delegation
    { delegPkh     :: PubKeyHash
    , delegTxRef   :: TxOutRef
    , delegCreated :: Slot
    , delegIp      :: Text
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DelegationConfig = DelegationConfig
    { dcNetworkId       :: NetworkId
    , dcMinTokenAmount  :: Integer
    , dcDelegationStart :: Slot
    , dcCurrencySymbol  :: CurrencySymbol
    } deriving (Show, Eq, Generic, FromJSON)

data DelegationHandle m = DelegationHandle
    { getResponses     :: m [KupoResponse]
    , getDatumByHash   :: DatumHash -> m (Maybe Datum)
    , getTokenBalance  :: CurrencySymbol -> TokenName -> StakingCredential -> m Integer
    , checkTxSignature :: TxId -> Address -> m Bool
    , mkLog            :: Text -> m ()
    , withResultSaving :: forall a. (FromJSON a, ToJSON a) => FilePath -> MaybeT m a -> MaybeT m a
    }

mkDelegationHandle :: DelegationConfig -> DelegationHandle IO
mkDelegationHandle d = DelegationHandle
    (getResponsesIO d)
    Kupo.getDatumByHash
    getTokenBalanceIO
    checkTxSignatureIO
    T.putStrLn
    Kupo.withResultSaving

getResponsesIO :: DelegationConfig -> IO [KupoResponse]
getResponsesIO DelegationConfig{..} = do
    slotTo <- utcToSlot <$> Time.getCurrentTime
    let req = def @(KupoRequest 'SUSpent 'CSCreated 'CSCreated)
    Kupo.partiallyGet dcNetworkId T.putStrLn dcDelegationStart slotTo 3600 req "delegation/responses_"

getTokenBalanceIO :: CurrencySymbol -> TokenName -> StakingCredential -> IO Integer
getTokenBalanceIO cs tokenName = Kupo.getTokenBalanceToSlot cs tokenName Nothing

checkTxSignatureIO :: TxId -> Address -> IO Bool
checkTxSignatureIO txId addr = case getStakeKey addr of
    Just (PubKeyHash pkh) -> txIsSignedByKey txId pkh
    _ -> pure False