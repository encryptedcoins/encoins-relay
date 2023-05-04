{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Encoins.Relay.Server.Server where

import           CSL                                      (TransactionUnspentOutputs)
import           CSL.Class                                (FromCSL (..))
import           Cardano.Server.Client.Client             (createServantClientEnv)
import           Cardano.Server.Config                    (decodeOrErrorFromFile)
import           Cardano.Server.Error                     (IsCardanoServerError (errMsg, errStatus), toEnvelope)
import           Cardano.Server.Input                     (InputContext (..))
import           Cardano.Server.Internal                  (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (..), ServerM,
                                                           getAuxillaryEnv)
import           Cardano.Server.Main                      (ServerApi)
import           Cardano.Server.Tx                        (mkTx)
import           Control.Exception                        (Exception, throw)
import           Control.Monad                            (void)
import           Control.Monad.Catch                      (MonadThrow (..))
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Default                             (def)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromJust)
import qualified Data.Text                                as T
import           ENCOINS.Core.OffChain                    (beaconTx, encoinsTx, postEncoinsPolicyTx, postStakingValidatorTx,
                                                           stakeOwnerTx)
import           ENCOINS.Core.OnChain                     (EncoinsRedeemer, beaconCurrencySymbol, encoinsSymbol,
                                                           ledgerValidatorAddress)
import           ENCOINS.Core.V1.OffChain                 (EncoinsMode (..))
import           Encoins.Relay.Verifier.Client            (verifierClient)
import           Encoins.Relay.Verifier.Server            (VerifierApiError (..), VerifierConfig (..), loadVerifierConfig)
import           Ledger                                   (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet                (getWalletAddr, getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder)
import           PlutusAppsExtra.Utils.Address            (bech32ToAddress)
import           PlutusTx.Prelude                         (BuiltinByteString, toBuiltin)
import           Servant.Client                           (BaseUrl (..), ClientEnv (..), Scheme (..))
import           Text.Hex                                 (Text, decodeHex)

verifierPKH :: BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

mkServerHandle :: IO (ServerHandle EncoinsApi)
mkServerHandle = do
    envTxOutRefs <- decodeOrErrorFromFile "txOutRef.json"
    VerifierConfig{..} <- loadVerifierConfig
    cEnv <- createServantClientEnv
    let envVerifierClientEnv = cEnv{baseUrl = BaseUrl Http (T.unpack cHost) cPort ""}
    pure $ ServerHandle
        Kupo
        EncoinsRelayEnv{..}
        getTrackedAddresses
        txBuilders
        (pure ())
        processRequest
        (const $ toEnvelope $ pure "Not implemented yet.")

type EncoinsApi = ServerApi
    ((EncoinsRedeemer, EncoinsMode), TransactionUnspentOutputs)
    EncoinsTxApiError
    ()
    '[]
    Text

type instance InputOf        EncoinsApi = (EncoinsRedeemer, EncoinsMode)
type instance AuxillaryEnvOf EncoinsApi = EncoinsRelayEnv

data EncoinsRelayEnv = EncoinsRelayEnv
    { envTxOutRefs         :: (TxOutRef, TxOutRef)
    , envVerifierClientEnv :: ClientEnv
    }

data EncoinsTxApiError
    = CorruptedExternalUTXOs
    | VerifierError VerifierApiError
    deriving (Show, Exception)

instance IsCardanoServerError EncoinsTxApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        CorruptedExternalUTXOs       -> "The request contained corrupted external UTXO data."
        VerifierError IncorrectInput -> "The request contained incorrect public input."
        VerifierError IncorrectProof -> "The request contained incorrect proof."

serverSetup :: ServerM EncoinsApi ()
serverSetup = void $ do
    (refStakeOwner, refBeacon) <- envTxOutRefs <$> getAuxillaryEnv
    -- Mint the stake owner token
    utxos <- getWalletUtxos
    let utxos' = Map.delete refBeacon utxos
    mkTx [] (InputContextServer utxos') [stakeOwnerTx refStakeOwner]
    -- Mint and send the beacon
    utxos'' <- getWalletUtxos
    mkTx [] (InputContextServer utxos'') [beaconTx refBeacon verifierPKH refStakeOwner]
    -- Define scripts' parameters
    let encoinsParams = (beaconCurrencySymbol refBeacon, verifierPKH)
        ledgerParams  = encoinsSymbol encoinsParams
    -- Post the ENCOINS minting policy
    mkTx [] def [postEncoinsPolicyTx encoinsParams referenceScriptSalt]
    -- Post the staking validator policy
    mkTx [] def [postStakingValidatorTx ledgerParams referenceScriptSalt]

getTrackedAddresses :: ServerM EncoinsApi [Address]
getTrackedAddresses = do
    (refStakeOwner, refBeacon) <- envTxOutRefs <$> getAuxillaryEnv
    let encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
        stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
    return [ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb), alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: ServerM EncoinsApi Address
getLedgerAddress = head <$> getTrackedAddresses

processRequest :: (InputOf EncoinsApi, TransactionUnspentOutputs) -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest ((red@((_, addr), _, _, _), mode), utxosCSL) = do
    let utxos   = maybe (throw CorruptedExternalUTXOs) Map.fromList $ fromCSL utxosCSL
        context = InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
    return ((red, mode), context)

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (red, mode) = do
    (refStakeOwner, refBeacon) <- envTxOutRefs <$> getAuxillaryEnv
    relayWalletAddress <- getWalletAddr
    let beaconSymb = beaconCurrencySymbol refBeacon
        stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
    red' <- verifyRedeemer red
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) (beaconSymb, verifierPKH) stakeOwnerSymb red' mode]

verifyRedeemer :: EncoinsRedeemer -> ServerM EncoinsApi EncoinsRedeemer
verifyRedeemer red = do
    verifierClientEnv <- envVerifierClientEnv <$> getAuxillaryEnv
    let ?servantClientEnv = verifierClientEnv
    liftIO (verifierClient red) >>= either throwM pure