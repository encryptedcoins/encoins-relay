module Encoins.Relay.Common.Error
    -- Error combinators
    ( Throws

    -- Envelope
    , Envelope
    , EndpointEnvelope
    , EndpointErrors
    , EndpointResult

    -- Error class
    , IsCardanoServerError (..)
    , toServantError

    -- Cardano server errors
    , ConnectionError (..)
    , BalanceExternalTxError (..)
    , InternalServerError (..)
    , MkTxError (..)
    , SubmitTxToLocalNodeError (..)
    , CslError (..)

    -- Other helpers functions
    , logCriticalExceptions
    , parseErrorText
    , throwMaybe
    , throwEither
    ) where

import           Encoins.Relay.Common.Error.Class        (CslError (..), InternalServerError (..), parseErrorText, IsCardanoServerError (..),
                                                   logCriticalExceptions, toServantError)
import           Encoins.Relay.Common.Error.Envelope
import           PlutusAppsExtra.Types.Error       (BalanceExternalTxError (..), ConnectionError (..), MkTxError (..),
                                                    SubmitTxToLocalNodeError (..), throwEither, throwMaybe)
