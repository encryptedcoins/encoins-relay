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
import           ENCOINS.Core.OffChain                    (beaconTx, encoinsTx, postEncoinsPolicyTx, postLedgerValidatorTx,
                                                           stakeOwnerTx)
import           ENCOINS.Core.OnChain                     (EncoinsRedeemer, ledgerValidatorAddress, EncoinsProtocolParams)
import           ENCOINS.Core.V1.OffChain                 (EncoinsMode (..))
import           Encoins.Relay.Verifier.Client            (mkVerifierClientEnv, verifierClient)
import           Encoins.Relay.Verifier.Server            (VerifierApiError (..))
import           Ledger                                   (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet                (getWalletAddr, getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder)
import           PlutusAppsExtra.Utils.Address            (bech32ToAddress)
import           PlutusTx.Prelude                         (BuiltinByteString, toBuiltin)
import           Servant.Client                           (ClientEnv (..))
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
    txOutRefs <- decodeOrErrorFromFile "txOutRef.json"
    let envEncoinsProtocolParams = (fst txOutRefs, snd txOutRefs, verifierPKH)
    envVerifierClientEnv <- mkVerifierClientEnv
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
    { envEncoinsProtocolParams :: EncoinsProtocolParams
    , envVerifierClientEnv     :: ClientEnv
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
    encoinsProtocolParams@(_, refBeacon, _) <- envEncoinsProtocolParams <$> getAuxillaryEnv
    -- Mint the stake owner token
    utxos <- getWalletUtxos
    let utxos' = Map.delete refBeacon utxos
    mkTx [] (InputContextServer utxos') [stakeOwnerTx encoinsProtocolParams]
    -- Mint and send the beacon
    utxos'' <- getWalletUtxos
    mkTx [] (InputContextServer utxos'') [beaconTx encoinsProtocolParams]
    -- Post the ENCOINS minting policy
    mkTx [] def [postEncoinsPolicyTx encoinsProtocolParams referenceScriptSalt]
    -- Post the staking validator policy
    mkTx [] def [postLedgerValidatorTx encoinsProtocolParams referenceScriptSalt]

getTrackedAddresses :: ServerM EncoinsApi [Address]
getTrackedAddresses = do
    encoinsProtocolParams <- envEncoinsProtocolParams <$> getAuxillaryEnv
    return [ledgerValidatorAddress encoinsProtocolParams, alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: ServerM EncoinsApi Address
getLedgerAddress = head <$> getTrackedAddresses

processRequest :: (InputOf EncoinsApi, TransactionUnspentOutputs) -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest ((red@((_, addr), _, _, _), mode), utxosCSL) = do
    let utxos   = maybe (throw CorruptedExternalUTXOs) Map.fromList $ fromCSL utxosCSL
        context = InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
    return ((red, mode), context)

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (red, mode) = do
    encoinsProtocolParams <- envEncoinsProtocolParams <$> getAuxillaryEnv
    relayWalletAddress <- getWalletAddr
    red' <- verifyRedeemer red
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) encoinsProtocolParams red' mode]

verifyRedeemer :: EncoinsRedeemer -> ServerM EncoinsApi EncoinsRedeemer
verifyRedeemer red = do
    verifierClientEnv <- envVerifierClientEnv <$> getAuxillaryEnv
    let ?servantClientEnv = verifierClientEnv
    liftIO (verifierClient red) >>= either throwM pure