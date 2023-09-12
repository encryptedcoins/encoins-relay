{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Encoins.Relay.Server.Delegation where

import           Cardano.Server.Config              (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Internal            (ServerM)
import           Cardano.Server.Tx                  (mkTx)
import           Control.Monad                      (void, when)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Control.Monad.State                (modify)
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Bifunctor                     (Bifunctor (..))
import           Data.Default                       (Default (..))
import           Data.Function                      (on)
import           Data.List.Extra                    (dropPrefix, dropSuffix, sortBy)
import           Data.Maybe                         (mapMaybe)
import           Data.Ord                           (Down (..))
import           Data.Text                          (Text)
import           Data.Time                          (getCurrentTime)
import qualified Data.Time                          as Time
import           Encoins.Relay.Server.Config        (EncoinsRelayConfig (..), loadEncoinsRelayConfig)
import           Encoins.Relay.Server.Server        (EncoinsApi)
import           GHC.Generics                       (Generic)
import           Ledger                             (Ada, Address (..), PaymentPubKeyHash (..), PubKeyHash, Slot, TxOutRef)
import qualified Ledger.Ada                         as Ada
import           Ledger.Constraints                 (mustPayToPubKeyAddress)
import           Plutus.V1.Ledger.Credential        (Credential (..), StakingCredential (..))
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Types.Tx           (TxConstructor (..))
import           System.Directory                   (getDirectoryContents)
import           Text.Read                          (readMaybe)

distributeRewards :: Config -> Ada -> ServerM EncoinsApi ()
distributeRewards config totalReward = void $ do
    relayConfig <- loadEncoinsRelayConfig config
    
    recepients <- liftIO $ do
        recepients <- getRecepients relayConfig totalReward
        putStrLn "Recepients:"
        mapM_ print recepients
        when (null recepients) $ error "No recepients found."
        pure recepients
    
    let mkConstr pkh scred ada = Just $ mustPayToPubKeyAddress (PaymentPubKeyHash pkh) scred (Ada.toValue ada)
        constrs = mconcat $ flip mapMaybe recepients $ \(addr, reward) -> case addr of
            Address (PubKeyCredential pkh) (Just scred) -> mkConstr pkh scred reward
            _   -> Nothing
    mkTx [] def $ pure $ modify $ \constr -> constr{txConstructorResult = Just (mempty, constrs)}

getRecepients :: EncoinsRelayConfig -> Ada -> IO [(Address, Ada)]
getRecepients EncoinsRelayConfig{..} totalReward = do
        (delegsFp, delegsTime) <- getLastDelegationsFile
        currentTime            <- getCurrentTime
        when (Time.diffUTCTime currentTime delegsTime > 3600) newDelegationsNotFoundErr
        delegs                 <- decodeOrErrorFromFile delegsFp
        putStrLn "Last found delegation file:"
        print delegs
        delegsWithTokenAmt     <- filter ((>= cDelegationMinTokenAmt) . snd) <$> addTokenAmt delegs
        print delegsWithTokenAmt
        let reward amt = fromInteger $ (amt * fromIntegral totalReward) `div` sum (map snd delegsWithTokenAmt)
        pure $ map (bimap delegAddress reward) delegsWithTokenAmt
    where
        getLastDelegationsFile = do
            delegFps <- getDirectoryContents cDelegationFolder
            case sortBy (compare `on` Down . snd) $ mapMaybe sequence (zip delegFps $ map extractTime delegFps) of
                r:_ -> pure $ first (\fp -> cDelegationFolder <> "/" <> fp) r
                _   -> delegationsNotFoundErr
        addTokenAmt = mapM $ \d -> (d,) <$> Kupo.getTokenBalanceToSlot cDelegationCurrencySymbol cDelegationTokenName Nothing (StakingHash $ PubKeyCredential $ delegStakeKey d)
        extractTime = readMaybe . dropSuffix ".json" . dropPrefix "delegators_"
        newDelegationsNotFoundErr = error "Can't get fresh delegations file. Maybe delegation app isn't running?"
        delegationsNotFoundErr    = error "Delegations file not found. Run the delegation app first \
                                       \and get the delegations file before reward distribution."

data Delegation = Delegation
    { delegCredential :: Credential
    , delegStakeKey   :: PubKeyHash
    , delegTxOutRef   :: TxOutRef
    , delegCreated    :: Slot
    , delegIp         :: Text
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

delegAddress :: Delegation -> Address
delegAddress d = Address (delegCredential d) (Just $ StakingHash $ PubKeyCredential $ delegStakeKey d)