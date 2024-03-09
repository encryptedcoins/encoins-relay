{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeApplications #-}

module Encoins.Relay.Server.Endpoints.Tx.New where

import           CSL                                        (TransactionInputs)
import           Cardano.Server.Error                       (BalanceExternalTxError, ConnectionError, IsCardanoServerError (..),
                                                             MkTxError, Throws)
import           Cardano.Server.Internal                    (AuxillaryEnvOf, ServerM)
import           Cardano.Server.Tx                          (mkBalanceTx)
import           Cardano.Server.Utils.Logger                (logMsg, (.<))
import           Control.Monad                              (join, liftM3)
import           Control.Monad.Catch                        (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Data.Aeson                                 (ToJSON)
import           Data.Text                                  (Text)
import qualified Data.Time                                  as Time
import           Encoins.Relay.Server.Endpoints.Tx.Intenral (EncoinsTxApiError, InputOfEncoinsApi, processRequest, txBuilders)
import           Encoins.Relay.Server.Internal              (EncoinsRelayEnv (..), getTrackedAddresses)
import           GHC.Generics                               (Generic)
import           Ledger                                     (CardanoTx, TxId (..), getCardanoTxId)
import           PlutusAppsExtra.Utils.Tx                   (cardanoTxToText)
import           PlutusTx.Prelude                           (fromBuiltin)
import           Servant                                    (JSON, Post, ReqBody, type (:>))
import           Text.Hex                                   (encodeHex)
import Cardano.Server.Handler (wrapHandler)

type NewTxApi = "newTx"
    :> Throws EncoinsTxApiError
    :> Throws NewTxApiError
    :> Throws ConnectionError
    :> Throws MkTxError
    :> Throws BalanceExternalTxError
    :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
    :> Post '[JSON] (Text, Text)

newtype NewTxApiError = UnserialisableCardanoTx CardanoTx
    deriving stock    (Show, Generic)
    deriving anyclass (ToJSON, Exception)

instance IsCardanoServerError NewTxApiError where
    errStatus _ = toEnum 422
    errMsg (UnserialisableCardanoTx tx) = "Cannot serialise balanced tx:" .< tx

newTxHandler :: AuxillaryEnvOf api ~ EncoinsRelayEnv =>
    (InputOfEncoinsApi, TransactionInputs) -> ServerM api (Text, Text)
newTxHandler req = withMetric "newTx request processing" $ wrapHandler @NewTxApi $ do
    logMsg $ "New newTx request received:\n" .< req
    (input, context) <- withMetric "processing request" $ processRequest req
    balancedTx       <- withMetric "balancing tx" $
        join $ liftM3 mkBalanceTx getTrackedAddresses (pure context) (txBuilders input)
    case cardanoTxToText balancedTx of
        Just res ->
            let txId = encodeHex $ fromBuiltin $ getTxId $ getCardanoTxId balancedTx
            in pure (txId, res)
        Nothing  -> throwM $ UnserialisableCardanoTx balancedTx
    where
        withMetric msg ma = do
            logMsg $ "start " <> msg
            start  <- liftIO Time.getCurrentTime
            res    <- ma
            finish <- res `seq` liftIO Time.getCurrentTime
            logMsg $ msg <> " finished in " .< Time.diffUTCTime finish start
            pure res