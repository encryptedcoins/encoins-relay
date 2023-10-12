{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

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
import           Data.Maybe                    (fromMaybe, catMaybes, mapMaybe, isNothing)
import           GHC.Generics                  (Generic)
import           Ledger                        (NetworkId, TxId (getTxId), fromCardanoValue)
import qualified Plutus.Script.Utils.Ada       as Ada
import           System.Random                 (Random (..))

import qualified CSL
import           CSL.Class                     (ToCSL (..))
import           ENCOINS.Core.OnChain          (minAdaTxOutInLedger)
import           Encoins.Relay.Server.Internal (EncoinsRelayEnv, getEncoinsSymbol, getLedgerUtxosKupo)
import qualified Plutus.Script.Utils.Value     as P
import           PlutusAppsExtra.Utils.Address (addressToBech32)
import           PlutusAppsExtra.Utils.Kupo    (KupoResponse (..))
import           PlutusTx.Builtins.Class       (FromBuiltin (..))
import           Text.Hex                      (encodeHex)
import qualified Debug.Trace as D

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
    kupoResponses <- getLedgerUtxosKupo
    when (null kupoResponses) $ throwM EmptyLedger
    let ada = maximum $ map (Ada.fromValue . fromCardanoValue . krValue) kupoResponses
    pure $ MaxAdaWithdrawResult $ subtract minAdaTxOutInLedger $ toInteger ada

getLedgerEncoins :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getLedgerEncoins = do
    ecs <- getEncoinsSymbol
    networkId <- getNetworkId
    let f = uncurry (&&) . (any ((== ecs) . (^. _1)) &&& (<= 6) . length) . P.flattenValue . fromCardanoValue . krValue
    LedgerUtxoResult . fromMaybe (throw CslConversionError) . toCSL . (,networkId) . filter f <$> getLedgerUtxosKupo

instance ToCSL ([KupoResponse], NetworkId) CSL.TransactionUnspentOutputs where
    toCSL (responses, networkId) = Just $ mapMaybe (toCSL . (, networkId)) responses

instance ToCSL (KupoResponse, NetworkId) CSL.TransactionUnspentOutput where
    toCSL (KupoResponse {..}, networkId) = do
        let input = CSL.TransactionInput (encodeHex $ fromBuiltin $ getTxId krTxId) krOutputIndex
            addr = addressToBech32 networkId krAddress
            val  = toCSL (fromCardanoValue krValue)

        when (isNothing addr) $ D.trace ("\n\n\n\naddr:" <> show addr <> "\n\n\n\n") $ pure ()
        when (isNothing val)  $ D.trace ("\n\n\n\nval:"  <> show val  <> "\n\n\n\n") $ pure ()

        output <- CSL.TransactionOutput <$> addr <*> val <*> Nothing <*> Nothing
        pure $ CSL.TransactionUnspentOutput input output
