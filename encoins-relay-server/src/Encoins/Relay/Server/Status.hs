{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Encoins.Relay.Server.Status where

import qualified CSL
import           CSL.Class                     (ToCSL (..))
import           Cardano.Server.Error          (ConnectionError, IsCardanoServerError (..))
import           Cardano.Server.Error.Servant  (Throws)
import           Cardano.Server.Handler        (wrapHandler)
import           Cardano.Server.Internal       (AuxillaryEnvOf, ServerM)
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
import           Encoins.Relay.Server.Internal (EncoinsRelayEnv (..), getEncoinsSymbol, getLedgerUtxos)
import           GHC.Generics                  (Generic)
import           Ledger                        (decoratedTxOutPlutusValue)
import qualified Plutus.Script.Utils.Ada       as Ada
import qualified Plutus.Script.Utils.Value     as P
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           Servant                       (JSON, Post, ReqBody, type (:>))
import           System.Random                 (Random (..))

type StatusApi = "status"
    :> Throws ConnectionError
    :> Throws EncoinsStatusError
    :> ReqBody '[JSON] EncoinsStatusReqBody
    :> Post '[JSON] EncoinsStatusResult

data EncoinsStatusReqBody
    -- | Get the maximum amount of ada that can be taken from a single utxo bound to a ledger address.
    = MaxAdaWithdraw
    -- | Get all ledger utxos containing 6 or fewer tokens (including ada) and at least one encoins token.
    | LedgerEncoins
    deriving (Show, Eq, Enum, Generic, FromJSON, ToJSON)

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
    restore 422 "Ledger doesn't have any utxos." = Just EmptyLedger
    restore 422 "Can't convert utxos to csl."    = Just CslConversionError
    restore _ _ = Nothing

encoinsStatusHandler :: (AuxillaryEnvOf api ~ EncoinsRelayEnv) =>
    EncoinsStatusReqBody -> ServerM api EncoinsStatusResult
encoinsStatusHandler req = wrapHandler @StatusApi $ case req of
    MaxAdaWithdraw -> getMaxAdaWithdraw
    LedgerEncoins  -> getLedgerEncoins

getMaxAdaWithdraw :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsStatusResult
getMaxAdaWithdraw = do
    utxos <- getLedgerUtxos mempty
    when (null utxos) $ throwM EmptyLedger
    let ada = maximum . map (Ada.fromValue . decoratedTxOutPlutusValue) $ Map.elems utxos
    pure $ MaxAdaWithdrawResult $ subtract minAdaTxOutInLedger $ toInteger ada

getLedgerEncoins :: (AuxillaryEnvOf api ~ EncoinsRelayEnv) => ServerM api EncoinsStatusResult
getLedgerEncoins = do
    ecs       <- getEncoinsSymbol
    networkId <- getNetworkId
    let f = uncurry (&&) . (any ((== ecs) . (^. _1)) &&& (<= 6) . length) . P.flattenValue . decoratedTxOutPlutusValue
    LedgerUtxoResult . fromMaybe (throw CslConversionError) . toCSL . (,networkId) . Map.filter f <$> getLedgerUtxos mempty