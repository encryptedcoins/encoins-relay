{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Encoins.Relay.Server.Status where

import           Cardano.Server.Error             (ConnectionError, Envelope, IsCardanoServerError (..), toEnvelope)
import           Cardano.Server.Internal          (AuxillaryEnvOf, ServerM)
import           Control.Lens                     ((^.))
import           Control.Lens.Tuple               (Field1 (_1))
import           Control.Monad                    (when)
import           Control.Monad.Catch              (Exception, MonadThrow (..))
import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import qualified Data.Map                         as Map
import           ENCOINS.Core.OnChain             (minAdaTxOutInLedger)
import           Encoins.Relay.Server.Internal    (EncoinsRelayEnv, getEncoinsSymbol, getLedgerUtxos)
import           GHC.Generics                     (Generic)
import           Ledger                           (DecoratedTxOut (..))
import qualified Ledger.Ada                       as Ada
import           Ledger.Value                     (flattenValue)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           System.Random                    (Random (..))

data EncoinsStatusReqBody
    -- | Get the maximum amount of ada that can be taken from a single utxo bound to a ledger address.
    = MaxAdaWithdrawRequest
    -- | Get all ledger utxos containing encoins tokens.
    | LedgerUtxoRequestEncoins
    -- | Get all ledger utxos containing 6 or less tokens (including ada).
    | LedgerUtxoRequestTokens
    deriving (Show, Eq, Enum, Generic, FromJSON, ToJSON)

instance Read EncoinsStatusReqBody where
    readsPrec _ = \case
        "ada"     -> [(MaxAdaWithdrawRequest,    "")]
        "encoins" -> [(LedgerUtxoRequestEncoins, "")]
        "tokens"  -> [(LedgerUtxoRequestTokens,  "")]
        _         -> []

instance Random EncoinsStatusReqBody where
    random g = let (i, g') = random g 
               in  (toEnum (i `mod` 3), g')
    randomR _ = random

data EncoinsStatusResult
    = MaxAdaWithdrawResult Integer
    | LedgerUtxoResult MapUTXO
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EncoinsStatusError
    = EmptyLedger
    deriving (Show, Eq, Exception)

instance IsCardanoServerError EncoinsStatusError where
    errStatus _ = toEnum 422
    errMsg EmptyLedger = "Ledger doesn't have any utxos."

type EncoinsStatusErrors = '[ConnectionError, EncoinsStatusError]

encoinsStatusHandler :: AuxillaryEnvOf api ~ EncoinsRelayEnv =>
    EncoinsStatusReqBody -> ServerM api (Envelope EncoinsStatusErrors EncoinsStatusResult)
encoinsStatusHandler = \case
    MaxAdaWithdrawRequest    -> toEnvelope getMaxAdaWithdraw
    LedgerUtxoRequestEncoins -> toEnvelope getLedgerEncoins
    LedgerUtxoRequestTokens  -> toEnvelope getLedgerTokens

getMaxAdaWithdraw :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getMaxAdaWithdraw = do
    utxos <- getLedgerUtxos
    when (null utxos) $ throwM EmptyLedger
    let ada = maximum . map (Ada.fromValue . _decoratedTxOutValue) $ Map.elems utxos
    pure $ MaxAdaWithdrawResult $ subtract minAdaTxOutInLedger . (* 1_000_000) $ toInteger ada

getLedgerEncoins :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getLedgerEncoins = do
    ecs <- getEncoinsSymbol
    filterLedgerUtxos $ any ((== ecs) . (^. _1)) . flattenValue . _decoratedTxOutValue

getLedgerTokens :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getLedgerTokens = filterLedgerUtxos $ (<= 6) . length . flattenValue . _decoratedTxOutValue

filterLedgerUtxos :: AuxillaryEnvOf api ~ EncoinsRelayEnv => (DecoratedTxOut -> Bool) -> ServerM api EncoinsStatusResult
filterLedgerUtxos f = LedgerUtxoResult . Map.filter f <$> getLedgerUtxos