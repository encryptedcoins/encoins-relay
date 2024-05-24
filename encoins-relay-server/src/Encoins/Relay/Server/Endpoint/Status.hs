{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards #-}

module Encoins.Relay.Server.Endpoint.Status where

import qualified CSL
import           CSL.Class                     (ToCSL (..))
import           Control.Arrow                 (Arrow ((&&&)))
import           Control.Exception             (throw)
import           Control.Lens                  ((^.))
import           Control.Lens.Tuple            (Field1 (_1))
import           Control.Monad                 (when)
import           Control.Monad.Catch           (Exception, MonadThrow (..))
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           ENCOINS.Core.OnChain          (minAdaTxOutInLedger)
import           Encoins.Relay.Common.Error    (ConnectionError, Envelope, IsCardanoServerError (..), Throws, toEnvelope)
import           Encoins.Relay.Server.Internal (EncoinsServerM, getEncoinsSymbol, getLedgerUtxos, EncoinsRelayEnv (..))
import           GHC.Generics                  (Generic)
import           Ledger                        (decoratedTxOutPlutusValue)
import qualified Plutus.Script.Utils.Ada       as Ada
import qualified Plutus.Script.Utils.Value     as P
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           Servant                       (JSON, Post, ReqBody, (:>))
import           System.Random                 (Random (..))
import Encoins.Common.Log (logInfo)
import Data.Text (Text)
import qualified Servant.Client as Servant
import Control.Monad.Reader (MonadReader(..), MonadIO (..))
import Encoins.Relay.Common.Server (mkServantClientEnv)
import Servant.Client (client)
import Data.Data (Proxy(..))

type StatusEndpoint
    = "status"
    :> Throws ConnectionError
    :> Throws EncoinsStatusError
    :> ReqBody '[JSON] EncoinsStatusReqBody
    :> Post '[JSON] EncoinsStatusResult

data EncoinsStatusReqBody
    -- | Get the maximum amount of ada that can be taken from a single utxo bound to a ledger address.
    = MaxAdaWithdraw
    -- | Get all ledger utxos containing 6 or fewer tokens (including ada) and at least one encoins token.
    | LedgerEncoins
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Read EncoinsStatusReqBody where
    readsPrec _ = \case
        "ada"     -> [(MaxAdaWithdraw, "")]
        "encoins" -> [(LedgerEncoins,  "")]
        _         -> []

instance Random EncoinsStatusReqBody where
    random g = let (b, g') = random g
               in (, g') $ if b then MaxAdaWithdraw else LedgerEncoins
    randomR _ = random

data EncoinsStatusResult
    = MaxAdaWithdrawResult Integer
    | LedgerUtxoResult CSL.TransactionUnspentOutputs
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EncoinsStatusError
    = EmptyLedger
    | CslConversionError
    deriving (Show, Eq, Exception)

instance IsCardanoServerError EncoinsStatusError where
    errStatus _ = toEnum 422
    errMsg  = \case
        EmptyLedger        -> "Ledger doesn't have any utxos."
        CslConversionError -> "Can't convert utxos to csl."

statusEndpointHandler :: EncoinsStatusReqBody -> EncoinsServerM (Envelope '[ConnectionError, EncoinsStatusError] EncoinsStatusResult)
statusEndpointHandler = \case
    MaxAdaWithdraw -> toEnvelope getMaxAdaWithdraw
    LedgerEncoins  -> toEnvelope getLedgerEncoins

getMaxAdaWithdraw :: EncoinsServerM EncoinsStatusResult
getMaxAdaWithdraw = do
    logInfo "Received `max ada withdraw` status request"
    utxos <- getLedgerUtxos mempty
    when (null utxos) $ throwM EmptyLedger
    let ada = maximum . map (Ada.fromValue . decoratedTxOutPlutusValue) $ Map.elems utxos
    pure $ MaxAdaWithdrawResult $ subtract minAdaTxOutInLedger $ toInteger ada

getLedgerEncoins :: EncoinsServerM EncoinsStatusResult
getLedgerEncoins = do
    logInfo "Received `ledger encoins` status request"
    ecs       <- getEncoinsSymbol
    networkId <- getNetworkId
    let f = uncurry (&&) . (any ((== ecs) . (^. _1)) &&& (<= 6) . length) . P.flattenValue . decoratedTxOutPlutusValue
    LedgerUtxoResult . fromMaybe (throw CslConversionError) . toCSL . (,networkId) . Map.filter f <$> getLedgerUtxos mempty

-- -- Check if status endpoint is alive
checkStatusEndpoint :: EncoinsServerM (Either Text ())
checkStatusEndpoint = do
    MkEncoinsRelayEnv{..} <- ask
    env <- mkServantClientEnv envPort envHost envHyperTextProtocol
    res <- liftIO $ Servant.runClientM (statusClient MaxAdaWithdraw) env
    either throwM (const $ pure $ Right ()) res

statusClient :: EncoinsStatusReqBody -> Servant.ClientM EncoinsStatusResult
statusClient = client (Proxy @StatusEndpoint)