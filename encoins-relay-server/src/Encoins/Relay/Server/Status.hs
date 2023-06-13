{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

module Encoins.Relay.Server.Status where

import           Cardano.Server.Error          (ConnectionError, Envelope, IsCardanoServerError (..), toEnvelope)
import           Cardano.Server.Internal       (AuxillaryEnvOf, ServerM, getNetworkId)
import           Control.Arrow                 (Arrow ((&&&)))
import           Control.Exception             (throw)
import           Control.Lens                  ((^.))
import           Control.Lens.Tuple            (Field1 (_1))
import           Control.Monad                 (when)
import           Control.Monad.Catch           (Exception, MonadThrow (..))
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           GHC.Generics                  (Generic)
import           Ledger                        (DecoratedTxOut (..))
import qualified Ledger.Ada                    as Ada
import           Ledger.Value                  (flattenValue)
import           System.Random                 (Random (..))

import qualified CSL
import           CSL.Class                     (toCSL)
import           ENCOINS.Core.OnChain          (minAdaTxOutInLedger)
import           Encoins.Relay.Server.Internal (EncoinsRelayEnv, getEncoinsSymbol, getLedgerUtxos)

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

type EncoinsStatusErrors = '[ConnectionError, EncoinsStatusError]

encoinsStatusHandler :: AuxillaryEnvOf api ~ EncoinsRelayEnv =>
    EncoinsStatusReqBody -> ServerM api (Envelope EncoinsStatusErrors EncoinsStatusResult)
encoinsStatusHandler = \case
    MaxAdaWithdraw -> toEnvelope getMaxAdaWithdraw
    LedgerEncoins  -> toEnvelope getLedgerEncoins

getMaxAdaWithdraw :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getMaxAdaWithdraw = do
    utxos <- getLedgerUtxos
    when (null utxos) $ throwM EmptyLedger
    let ada = maximum . map (Ada.fromValue . _decoratedTxOutValue) $ Map.elems utxos
    pure $ MaxAdaWithdrawResult $ subtract minAdaTxOutInLedger . (* 1_000_000) $ toInteger ada

getLedgerEncoins :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getLedgerEncoins = do
    ecs <- getEncoinsSymbol
    networkId <- getNetworkId
    let f = uncurry (&&) . (any ((== ecs) . (^. _1)) &&& (<= 6) . length) . flattenValue . _decoratedTxOutValue
    LedgerUtxoResult . fromMaybe (throw CslConversionError) . toCSL . (,networkId) . Map.filter f <$> getLedgerUtxos
