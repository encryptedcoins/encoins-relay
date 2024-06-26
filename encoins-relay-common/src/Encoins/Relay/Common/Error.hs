module Encoins.Relay.Common.Error
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
    , SubmitTxError (..)
    , CslError (..)
      -- Other helpers functions
    , logCriticalExceptions
    , parseErrorText
    , throwMaybe
    , throwEither
    ) where

import           Encoins.Relay.Common.Error.Class    (CslError (..), InternalServerError (..), IsCardanoServerError (..),
                                                      logCriticalExceptions, parseErrorText, toServantError)
import           Encoins.Relay.Common.Error.Envelope
import           PlutusAppsExtra.Types.Error         (BalanceExternalTxError (..), ConnectionError (..), MkTxError (..), SubmitTxError (..),
                                                      throwEither, throwMaybe)
