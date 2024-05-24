{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}

module Encoins.Relay.Common.Error.Class where

import           Control.Exception                    (Exception, SomeException)
import           Control.Lens                         ((^?))
import           Data.Aeson                           (KeyValue ((.=)))
import qualified Data.Aeson                           as J
import           Data.Aeson.Lens                      (_String, key)
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Encoins.Common.Log
import           Encoins.Common.Transform
import           Katip
import           Network.HTTP.Types                   (Status)
import qualified Network.HTTP.Types.Header
import           PlutusAppsExtra.Api.Kupo             (pattern KupoConnectionError)
import           PlutusAppsExtra.IO.ChainIndex.Plutus (pattern PlutusChainIndexConnectionError)
import           PlutusAppsExtra.IO.Wallet.Cardano    (pattern CardanoWalletApiConnectionError)
import           PlutusAppsExtra.Types.Error          (BalanceExternalTxError (..), ConnectionError (..), MkTxError (..),
                                                       SubmitTxToLocalNodeError (..))
import qualified Servant

class Exception e => IsCardanoServerError e where

    errStatus :: e -> Status

    errStatusText :: e -> Text
    errStatusText = statusTextFromStatus . errStatus

    errMsg :: e -> Text

    errBody :: e -> LBS.ByteString
    errBody e = J.encode $ cardanoServerErrorToJSON e

    errHeaders :: e -> [Network.HTTP.Types.Header.Header]
    errHeaders _ = [(Network.HTTP.Types.Header.hContentType, "application/json")]

cardanoServerErrorToJSON :: IsCardanoServerError e => e -> J.Value
cardanoServerErrorToJSON e = J.object
    [ "errCode" .= fromEnum (errStatus e)
    , "errMsg" .= errMsg e
    ]

parseErrorText :: J.Value -> Maybe Text
parseErrorText = (^? key "errMsg" . _String)

toServantError :: forall e. IsCardanoServerError e => e -> Servant.ServerError
toServantError e = Servant.ServerError
    (fromEnum $ errStatus e)
    (T.unpack $ errStatusText e)
    (errBody e)
    (errHeaders e)

statusTextFromStatus :: Status -> Text
statusTextFromStatus s = case fromEnum s of
        300 -> getStatusText Servant.err300
        301 -> getStatusText Servant.err301
        302 -> getStatusText Servant.err302
        303 -> getStatusText Servant.err303
        304 -> getStatusText Servant.err304
        305 -> getStatusText Servant.err305
        307 -> getStatusText Servant.err307
        400 -> getStatusText Servant.err400
        401 -> getStatusText Servant.err401
        402 -> getStatusText Servant.err402
        403 -> getStatusText Servant.err403
        404 -> getStatusText Servant.err404
        405 -> getStatusText Servant.err405
        406 -> getStatusText Servant.err406
        407 -> getStatusText Servant.err407
        409 -> getStatusText Servant.err409
        410 -> getStatusText Servant.err410
        411 -> getStatusText Servant.err411
        412 -> getStatusText Servant.err412
        413 -> getStatusText Servant.err413
        414 -> getStatusText Servant.err414
        415 -> getStatusText Servant.err415
        416 -> getStatusText Servant.err416
        417 -> getStatusText Servant.err417
        418 -> getStatusText Servant.err418
        422 -> getStatusText Servant.err422
        500 -> getStatusText Servant.err500
        501 -> getStatusText Servant.err501
        502 -> getStatusText Servant.err502
        503 -> getStatusText Servant.err503
        504 -> getStatusText Servant.err504
        505 -> getStatusText Servant.err505
        _   -> ""
    where
        getStatusText = T.pack . Servant.errReasonPhrase

instance IsCardanoServerError SomeException where
    errStatus _ = toEnum 500
    errMsg e = "Unhandled exception:\n" <> toText e

data InternalServerError
    = NoWalletProvided
    | NoMaestroToken
    deriving (Show, Exception)

instance IsCardanoServerError ConnectionError where
    errStatus _ = toEnum 503
    errMsg = \case
        PlutusChainIndexConnectionError{} -> toMsg "Caradno chain index API"
        KupoConnectionError{}             -> toMsg "Kupo chain index API"
        CardanoWalletApiConnectionError{} -> toMsg "Cardano wallet API"
        _                                 -> toMsg "Some external API"
        where toMsg = (<> " is currently unavailable. Try again later.")

instance IsCardanoServerError MkTxError where
    errStatus _ = toEnum 422
    errMsg e = "The requested transaction could not be built. Reason:\n" <> toText e

instance IsCardanoServerError BalanceExternalTxError where
    errStatus _ = toEnum 422
    errMsg e = "The requested transaction could not be built. Reason: " <> case e of
        MakeUnbalancedTxError{}
            -> "Unable to build an UnbalancedTx."
        NonBabbageEraChangeAddress{}
            -> "Change address is not from Babbage era."
        MakeUtxoProviderError err _
            -> "Unable to extract an utxoProvider from wallet outputs:\n" <> toText err
        MakeAutoBalancedTxError err _
            -> "Unable to build an auto balanced tx:\n" <> toText err

instance IsCardanoServerError SubmitTxToLocalNodeError where
    errStatus = \case
        NoConnectionToLocalNode{} -> toEnum 503
        FailedSumbit{}            -> toEnum 422
    errMsg = \case
        NoConnectionToLocalNode{} -> "Server local node is currently unavailable."
        FailedSumbit err          -> "An error occurred while sending tx to local node. Reason: " <> toText err

data CslError
    = CslConversionError
    deriving (Show, Exception)

instance IsCardanoServerError CslError where
    errStatus _ = toEnum 422
    errMsg  = \case
        CslConversionError -> "An error occurred while converting plutus data to csl."

-- This function helps to skip non-critical exceptions (most of them are related to warp/wai) and log only critical ones
logCriticalExceptions :: forall e m. (Exception e, KatipContext m) => e -> m ()
logCriticalExceptions e
    | show e == "Warp: Client closed connection prematurely" = pure ()
    | show e == "Thread killed by timeout manager"           = pure ()
    | otherwise = do
        logError "Unhandled exception:"
        logErrorS True e
