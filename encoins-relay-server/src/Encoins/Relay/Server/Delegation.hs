{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Encoins.Relay.Server.Delegation where

import           Cardano.Api                          (writeFileJSON)
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Cardano.Server.Internal              (ServerM, getAuxillaryEnv)
import           Cardano.Server.Tx                    (mkTx)
import           Control.Applicative                  ((<|>))
import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.State                  (modify)
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Default                         (Default (..))
import           Data.Function                        (on)
import           Data.Functor                         ((<&>))
import           Data.List.Extra                      (chunksOf, partition, sort)
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import           Data.Maybe                           (mapMaybe)
import           Encoins.Relay.Apps.Delegation.Client (mkDelegationClientEnv, serverDelegatesClient)
import           Encoins.Relay.Server.Internal        (EncoinsRelayEnv (..))
import           Encoins.Relay.Server.Server          (EncoinsApi)
import           Ledger                               (Address (..), PaymentPubKeyHash (..), minAdaTxOutEstimated)
import           Ledger.Tx.Constraints                (mustPayToPubKeyAddress)
import           Plutus.Script.Utils.Ada              (Ada)
import qualified Plutus.Script.Utils.Ada              as Ada
import           Plutus.V2.Ledger.Api                 (Credential (..))
import           PlutusAppsExtra.Types.Tx             (TxConstructor (..))
import           PlutusAppsExtra.Utils.Address        (bech32ToAddress)

distributeRewards :: Ada -> ServerM EncoinsApi ()
distributeRewards totalReward = void $ do
        distribution <- getRewardsDistribution totalReward
        debts <- liftIO $ decodeOrErrorFromFile debtFile <|> pure []
        let (distribution', debts') = partition ((> minAdaTxOutEstimated) . snd) $ concatDistrubutions distribution debts
        when (null distribution' && null debts') $ error "Server doesn't has any delegates."
        mapM_ sendFunds $ mkConstrs distribution'
        liftIO $ writeFileJSON debtFile debts'
    where
        mkConstrs recepients = fmap mconcat $ chunksOf 5 $ flip mapMaybe recepients $ \(addr, reward) -> case addr of
            Address (PubKeyCredential pkh) (Just scred) -> Just $ mustPayToPubKeyAddress (PaymentPubKeyHash pkh) scred (Ada.toValue reward)
            _ -> Nothing

        sendFunds constrs = mkTx [] def (pure $ modify $ \constr -> constr{txConstructorResult = Just (mempty, constrs)})

        debtFile = "debts.json"

getRewardsDistribution :: Ada -> ServerM EncoinsApi [(Address, Ada)]
getRewardsDistribution totalReward = calculateReward <$> getRecepients
    where
        getRecepients = mapMaybe (\(addrTxt, b) -> (, b) <$> bech32ToAddress addrTxt) <$> do
            EncoinsRelayEnv{..} <- getAuxillaryEnv
            clientEnv <- liftIO $ mkDelegationClientEnv envDelegationSeverHost envDelegationServerPort envDelegationServerProtocol
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
