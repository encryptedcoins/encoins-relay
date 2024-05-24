{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Encoins.Relay.Server.Endpoint.Utxos where

import qualified CSL
import           CSL.Class                     (toCSL)
import           Control.Exception             (Exception (..), SomeException, throw)
import           Control.Monad.Catch           (try)
import           Data.Aeson                    (ToJSON)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           PlutusAppsExtra.IO.ChainIndex (getUtxosAt)
import           PlutusAppsExtra.Types.Tx      (allRequirements)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           Servant                       (Get, JSON, ReqBody, (:>))
import Encoins.Relay.Common.Error (Throws, ConnectionError, CslError (..), IsCardanoServerError (..), Envelope, toEnvelope)
import Encoins.Relay.Server.Internal
import Encoins.Common.Log (logInfo, logInfoS)
import PlutusAppsExtra.Utils.Network (HasNetworkId(..))
type UtxosEndpoint
    =  "utxos"
    :> Throws UtxosError
    :> Throws ConnectionError
    :> Throws CslError
    :> ReqBody '[JSON] Text
    :> Get '[JSON] CSL.TransactionUnspentOutputs

data UtxosError
    = UnparsableAddress
    deriving (Show, Exception, Generic, ToJSON)

instance IsCardanoServerError UtxosError where
    errStatus _ = toEnum 400
    errMsg = \case
        UnparsableAddress -> "Incorrect wallet address."

utxosEndpointHandler :: Text -> EncoinsServerM (Envelope '[UtxosError, ConnectionError, CslError] CSL.TransactionUnspentOutputs)
utxosEndpointHandler addrTxt = toEnvelope $ do
    logInfo "New utxos request received:\n"
    logInfoS True addrTxt
    -- checkEndpointAvailability UtxosE
    let !addr = fromMaybe (throw UnparsableAddress) $ bech32ToAddress addrTxt
    networkId <- getNetworkId
    fromMaybe (throw CslConversionError) . toCSL . (, networkId) .
        either mempty id <$> try @_ @SomeException (getUtxosAt allRequirements addr)
