{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Encoins.Relay.Server.Delegation where

import           Cardano.Api                          (writeFileJSON)
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Cardano.Server.Internal              (Env (..), ServerM, getAuxillaryEnv, mkServantClientEnv)
import           Cardano.Server.Tx                    (mkBalanceTx, mkTx)
import           Cardano.Server.Utils.Logger          (logMsg, logPretty)
import           Control.Applicative                  ((<|>))
import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (asks)
import           Control.Monad.State                  (modify)
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Char                            (toUpper)
import           Data.Default                         (Default (..))
import           Data.Function                        (on)
import           Data.Functor                         ((<&>))
import           Data.List.Extra                      (chunksOf, partition, sort)
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import           Data.Maybe                           (mapMaybe)
import qualified Data.Text                            as T
import           Encoins.Relay.Apps.Delegation.Client (serverDelegatesClient)
import           Encoins.Relay.Server.Internal        (EncoinsRelayEnv (..))
import           Encoins.Relay.Server.Server          (EncoinsApi)
import           Ledger                               (Address (..), PaymentPubKeyHash (..), minAdaTxOutEstimated)
import           Ledger.Tx.Constraints                (mustPayToPubKeyAddress)
import           Plutus.Script.Utils.Ada              (Ada)
import qualified Plutus.Script.Utils.Ada              as Ada
import           Plutus.V2.Ledger.Api                 (Credential (..))
import           PlutusAppsExtra.IO.Wallet            (HasWalletProvider (..))
import           PlutusAppsExtra.Types.Tx             (TxConstructor (..))
import           PlutusAppsExtra.Utils.Address        (bech32ToAddress)

distributeRewards :: Ada -> ServerM EncoinsApi ()
distributeRewards totalReward = void $ do
        distribution <- getRewardsDistribution totalReward
        debts <- liftIO $ decodeOrErrorFromFile debtFile <|> pure []
        let (distribution', debts') = partition ((> minAdaTxOutEstimated) . snd) $ concatDistrubutions distribution debts
        when (null distribution' && null debts') $ error "Server doesn't has any delegates."
        addrs <- getWalletAddresses
        mapM_ (sendFunds addrs) $ mkConstrs distribution'
        liftIO $ writeFileJSON debtFile debts'
    where
        mkConstrs recepients = fmap mconcat $ chunksOf maxParticipants $ flip mapMaybe recepients $ \(addr, reward) -> case addr of
            Address (PubKeyCredential pkh) (Just scred) -> Just $ mustPayToPubKeyAddress (PaymentPubKeyHash pkh) scred (Ada.toValue reward)
            _ -> Nothing

        sendFunds addrs constrs = do
            let txBuilder = [modify $ \constr -> constr{txConstructorResult = Just (mempty, constrs)}]
            tx <- mkBalanceTx addrs def txBuilder
            logPretty tx
            logMsg $ "Please enter \"Y\" to complete the above transaction or \"N\" to cancel.\n\
                   \Keep in mind that each transaction contains no more than "
                   <> T.pack (show maxParticipants)
                   <> " participants."
            doConfirmation
            mkTx addrs def txBuilder

        debtFile = "debts.json"

        doConfirmation = do
            map toUpper <$> liftIO getLine >>= \case
                "Y" -> pure ()
                "N" -> error "Interrupted."
                _   -> doConfirmation

maxParticipants :: Int
maxParticipants = 5

getRewardsDistribution :: Ada -> ServerM EncoinsApi [(Address, Ada)]
getRewardsDistribution totalReward = calculateReward <$> getRecepients
    where
        getRecepients = mapMaybe (\(addrTxt, b) -> (, b) <$> bech32ToAddress addrTxt) <$> do
            EncoinsRelayEnv{..} <- getAuxillaryEnv
            creds <- asks envCreds
            let ?creds = creds
            clientEnv <- mkServantClientEnv envDelegationServerPort envDelegationSeverHost envDelegationServerProtocol
            let ?servantClientEnv = clientEnv
            either (error . show) Map.toList <$> liftIO (serverDelegatesClient envDelegationIp)

        calculateReward recepients =
            let totalBalance = sum $ snd <$> recepients
            in recepients <&> second (fromInteger . (`div` totalBalance) . (* fromIntegral totalReward))

concatDistrubutions :: [(Address, Ada)] -> [(Address, Ada)] -> [(Address, Ada)]
concatDistrubutions distribution debts
    = map (\xs -> (fst $ NonEmpty.head xs, sum $ snd <$> xs))
    $ NonEmpty.groupBy ((==) `on` fst)
    $ sort
    $ distribution <> debts
