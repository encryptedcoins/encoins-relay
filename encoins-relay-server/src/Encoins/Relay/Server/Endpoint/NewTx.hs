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


module Encoins.Relay.Server.Endpoint.NewTx where

import           CSL                                    (TransactionInputs)

import           Control.Monad.Catch                    (Exception, MonadThrow (throwM))
import           Data.Aeson                             (ToJSON)
import           Data.Text                              (Text)
import           Encoins.Common.Log                     (logInfo, logInfoS)
import           Encoins.Common.Transform               (toText)
import           Encoins.Relay.Common.Error             (BalanceExternalTxError, ConnectionError,
                                                         IsCardanoServerError (errMsg, errStatus), MkTxError, Throws)
import           Encoins.Relay.Common.Tx                (mkBalanceTx)
import           Encoins.Relay.Server.Endpoint.ServerTx (EncoinsTxApiError, processRequest, txBuilders)
import           Encoins.Relay.Server.Internal          (EncoinsServerInput, EncoinsServerM, getTrackedAddresses)
import           GHC.Generics                           (Generic)
import           Ledger                                 (CardanoTx, TxId (..), getCardanoTxId)
import           PlutusAppsExtra.Utils.Tx               (cardanoTxToText)
import           PlutusTx.Prelude                       (fromBuiltin)
import           Servant                                (JSON, Post, ReqBody, type (:>))
import           Text.Hex                               (encodeHex)
import Encoins.Relay.Common.Server.Handler (wrapHandler)

type NewTxEndpoint = "newTx"
    :> Throws EncoinsTxApiError
    :> Throws NewTxApiError
    :> Throws ConnectionError
    :> Throws MkTxError
    :> Throws BalanceExternalTxError
    :> ReqBody '[JSON] (EncoinsServerInput, TransactionInputs)
    :> Post '[JSON] (Text, Text)

newtype NewTxApiError = UnserialisableCardanoTx CardanoTx
    deriving stock    (Show, Generic)
    deriving anyclass (ToJSON, Exception)

instance IsCardanoServerError NewTxApiError where
    errStatus _ = toEnum 422
    errMsg (UnserialisableCardanoTx tx) = "Cannot serialise balanced tx:\n" <> toText tx

newTxEndpointHandler :: (EncoinsServerInput, TransactionInputs)
    -> EncoinsServerM (Text, Text)
newTxEndpointHandler req = wrapHandler @NewTxEndpoint $ do
    logInfo "New newTx request received:"
    logInfoS True req
    (input, context) <- processRequest req
    addrs <- getTrackedAddresses
    txBuilder <- txBuilders input
    balancedTx <- mkBalanceTx addrs context txBuilder Nothing
    case cardanoTxToText balancedTx of
        Just res ->
            let txId = encodeHex $ fromBuiltin $ getTxId $ getCardanoTxId balancedTx
            in pure (txId, res)
        Nothing -> throwM $ UnserialisableCardanoTx balancedTx
