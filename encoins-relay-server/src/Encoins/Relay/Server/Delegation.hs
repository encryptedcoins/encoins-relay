{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Encoins.Relay.Server.Delegation where

import           Cardano.Api                              (writeFileJSON)
import           Control.Applicative                      ((<|>))
import           Control.Monad                            (void, when)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Control.Monad.Reader                     (MonadReader (..), asks)
import           Control.Monad.State                      (modify)
import           Data.Bifunctor                           (Bifunctor (..))
import           Data.Char                                (toUpper)
import           Data.Default                             (Default (..))
import           Data.Function                            (on)
import           Data.Functor                             ((<&>))
import           Data.List.Extra                          (chunksOf, partition, sort)
import qualified Data.List.NonEmpty                       as NonEmpty
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromMaybe, mapMaybe)
import qualified Data.Text                                as T
import           Encoins.Common.Constant                  (newLine)
import           Encoins.Common.Log                       (logInfo, logInfoS)
import           Encoins.Relay.Common                     (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Server              (mkServantClientEnv)
import           Encoins.Relay.Common.Tx                  (mkBalanceTx, mkTx)
import           Encoins.Relay.Common.Tx.Input            (InputContext (..))
import           Encoins.Relay.Delegation.Client          (serverDelegatesClient)
import           Encoins.Relay.Server.Internal            (EncoinsRelayEnv (..), EncoinsServerM)
import           Ledger                                   (Address (..), PaymentPubKeyHash (..), TxOutRef (TxOutRef), minAdaTxOutEstimated)
import           Plutus.Script.Utils.Ada                  (Ada)
import qualified Plutus.Script.Utils.Ada                  as Ada
import           PlutusAppsExtra.IO.Wallet                (HasWallet (..), getWalletUtxos)
import           PlutusAppsExtra.PlutusApps.TxConstraints (mustPayToPubKeyAddress)
import           PlutusAppsExtra.Types.Tx                 (TxConstructor (..))
import           PlutusAppsExtra.Utils.Address            (addressToBech32, bech32ToAddress)
import           PlutusAppsExtra.Utils.Network            (HasNetworkId (..))
import           PlutusAppsExtra.Utils.Tx                 (mkCip20Metadata)
import           PlutusLedgerApi.V2                       (Credential (..))

distributeRewards :: Ada -> EncoinsServerM ()
distributeRewards totalReward = void $ do
        distribution <- getRewardsDistribution totalReward
        debts <- liftIO $ decodeOrErrorFromFile debtFile <|> pure []
        let (distribution', debts') = partition ((> minAdaTxOutEstimated) . snd) $ concatDistrubutions distribution debts
        when (null distribution' && null debts') $ error "Server doesn't has any delegates."
        let constrsWithRewards = mkConstrsWithReward distribution'
        mkConfirmationMsg distribution' debts' (snd <$> constrsWithRewards)
        doConfirmation
        mapM_ (uncurry sendFunds) constrsWithRewards
        liftIO $ writeFileJSON debtFile debts'
    where
        mkConstrsWithReward recepients = fmap mconcat $ chunksOf maxParticipants $ flip mapMaybe recepients $ \(addr, reward) -> case addr of
            Address (PubKeyCredential pkh) (Just scred) -> Just $ (, reward) $ mustPayToPubKeyAddress (PaymentPubKeyHash pkh) scred (Ada.toValue reward)
            _ -> Nothing

        sendFunds constrs reward = do
            relayUrl <- asks envDelegationIp
            utxos <- getWalletUtxos mempty
            addrs <- getWalletAddresses
            metaData <- either (error . show) (pure . Just) $ mkCip20Metadata ["ENCOINS", "Reward", relayUrl, T.pack $ show $ toInteger reward]
            let txBuilder = [modify $ \constr -> constr{txConstructorResult = Just (mempty, constrs)}]
                ctx = InputContextClient mempty utxos (TxOutRef "" 0) (NonEmpty.head addrs)
            tx <- mkBalanceTx (NonEmpty.toList addrs) ctx txBuilder metaData
            logInfoS True tx
            mkTx (NonEmpty.toList addrs) def txBuilder metaData

        debtFile = "debts.json"

        mkConfirmationMsg distribution debts rewards = do
            rewardUrl <- asks envDelegationIp
            networkId <- getNetworkId
            let txsNum = length rewards
                onMultipleTxs single multiple = if txsNum == 1 then single else multiple
                txs = T.pack (show txsNum) <> " transaction" <> onMultipleTxs "" "s"
                reward = renderAda (sum rewards)
                rewardSplitted =  " (" <> T.intercalate ", " (map renderAda rewards) <> ")"
                renderedDistribution = fmap (\(a,r) -> fromMaybe (T.pack $ show a) (addressToBech32 networkId a) <> " : " <> renderAda r)
                debtsMsg = case debts of
                    [] -> ""
                    _  -> "\n\nRewards of these addresses are too small to be included. \
                           \They will be added to debt file and rewarded in next distributions:\n"
                        <> T.intercalate "\n" (renderedDistribution debts)
            logInfo $ mconcat
                [ "Please enter \"Y\" to complete ", txs
                , " and distribute rewards - ", reward, onMultipleTxs "" rewardSplitted
                , " between ", rewardUrl, " delegates "
                , "or \"N\" to cancel."
                , newLine
                , "Keep in mind that each transaction contains no more than "
                , T.pack (show maxParticipants), " participants."
                , newLine
                , newLine
                , rewardUrl, " delegates:"
                , newLine
                , T.intercalate newLine $ renderedDistribution distribution
                , debtsMsg
                ]

        renderAda = T.pack . (\(f, w) -> case w of {"" -> "0"; _ -> w}  <> "." <> f <> "₳")
            . bimap reverse reverse . splitAt 6 . reverse . show . toInteger

        doConfirmation = do
            map toUpper <$> liftIO getLine >>= \case
                "Y" -> pure ()
                "N" -> error "Interrupted."
                _   -> doConfirmation

maxParticipants :: Int
maxParticipants = 5

getRewardsDistribution :: Ada -> EncoinsServerM [(Address, Ada)]
getRewardsDistribution totalReward = calculateReward <$> getRecepients
    where
        getRecepients = mapMaybe (\(addrTxt, b) -> (, b) <$> bech32ToAddress addrTxt) <$> do
            MkEncoinsRelayEnv{..} <- ask
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
