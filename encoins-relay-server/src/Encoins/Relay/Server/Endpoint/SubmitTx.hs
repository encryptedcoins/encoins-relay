{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Encoins.Relay.Server.Endpoint.SubmitTx where

import           Control.Monad.Catch           (Exception, MonadThrow (throwM))
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Either.Extra             (maybeToEither)
import           Data.Text                     (Text)
import           Encoins.Common.Log            (logInfo, logInfoS)
import           Encoins.Common.Transform      (toText)
import           Encoins.Relay.Common.Error    (ConnectionError, Envelope, IsCardanoServerError (errMsg, errStatus),
                                                SubmitTxToLocalNodeError, Throws, toEnvelope)
import           Encoins.Relay.Server.Internal (EncoinsServerM)
import           GHC.Generics                  (Generic)
import           Ledger                        (CardanoTx)
import           Ledger.Crypto                 (PubKey, Signature)
import           PlutusAppsExtra.IO.Tx         (sumbitTxToNodeLocal)
import           PlutusAppsExtra.Utils.Tx      (addCardanoTxSignature, textToCardanoTx, textToPubkey, textToSignature)
import           Servant                       (JSON, NoContent (..), Post, ReqBody, (:>))

type SubmitTxEndpoint
    = "submitTx"
    :> Throws SubmitTxApiError
    :> Throws SubmitTxToLocalNodeError
    :> Throws ConnectionError
    :> ReqBody '[JSON] SubmitTxReqBody
    :> Post '[JSON] NoContent

data SubmitTxReqBody = SubmitTxReqBody
    { submitReqTx        :: Text
    , submitReqWitnesses :: [(Text, Text)]
    } deriving (Show, Read, Generic, ToJSON, FromJSON)

data SubmitTxApiError = UnparsableTx Text
                      | UnparsableWitnesses [(Text, Text)]
    deriving (Show, Generic, ToJSON)
    deriving Exception

instance IsCardanoServerError SubmitTxApiError where
    errStatus _ = toEnum 400
    errMsg (UnparsableTx tx)          = "Cannot parse CardanoTx from hex:\n" <> toText tx
    errMsg (UnparsableWitnesses wtns) = "Cannot parse witnesses from hex:\n" <> toText wtns

submitTxEndpointHandler :: SubmitTxReqBody
    -> EncoinsServerM (Envelope '[SubmitTxApiError, SubmitTxToLocalNodeError, ConnectionError] NoContent)
submitTxEndpointHandler req = toEnvelope $ do
    logInfo "New submitTx request received:"
    logInfoS True req
    -- checkEndpointAvailability SubmitTxE
    (ctx, wtns) <- either throwM pure $ parseSubmitTxReqBody req
    let ctx' = foldr (uncurry addCardanoTxSignature) ctx wtns
    sumbitTxToNodeLocal ctx'
    pure NoContent

parseSubmitTxReqBody :: SubmitTxReqBody -> Either SubmitTxApiError (CardanoTx, [(PubKey, Signature)])
parseSubmitTxReqBody SubmitTxReqBody{..} = do
    ctx  <- maybeToEither (UnparsableTx submitReqTx) $ textToCardanoTx submitReqTx
    wtns <- maybeToEither (UnparsableWitnesses submitReqWitnesses) $ mapM parseWitness submitReqWitnesses
    pure (ctx, wtns)
  where
    parseWitness (pkText, sigText) = do
        pk  <- textToPubkey pkText
        sig <- textToSignature sigText
        return (pk, sig)
