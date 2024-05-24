module Encoins.Relay.Common.Error
    -- Error combinators
    ( Throws
    , Envelope
    , toEnvelope

    -- Error class
    , IsCardanoServerError (..)

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
                                                   logCriticalExceptions)
import           Encoins.Relay.Common.Error.Servant      (Envelope, Throws)
import           Encoins.Relay.Common.Error.ToEnvelope   (toEnvelope)
import           PlutusAppsExtra.Types.Error       (BalanceExternalTxError (..), ConnectionError (..), MkTxError (..),
                                                    SubmitTxToLocalNodeError (..), throwEither, throwMaybe)
