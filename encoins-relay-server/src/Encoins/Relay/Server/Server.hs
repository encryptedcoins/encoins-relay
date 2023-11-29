{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Encoins.Relay.Server.Server where

import           CSL                            (TransactionInputs)
import qualified CSL
import           CSL.Class                      (FromCSL (..))
import           Cardano.Server.Client.Internal (statusC)
import           Cardano.Server.Config          (Config (..))
import           Cardano.Server.Error           (IsCardanoServerError (errMsg, errStatus))
import           Cardano.Server.Input           (InputContext (..))
import           Cardano.Server.Internal        (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (..), ServerM,
                                                 getAuxillaryEnv, mkServerClientEnv, mkServantClientEnv)
import           Cardano.Server.Main            (ServerApi)
import           Cardano.Server.Tx              (mkTx)
import           Control.Arrow                  ((&&&))
import           Control.Exception              (Exception, throw)
import           Control.Monad                  (void)
import           Control.Monad.Catch            (MonadThrow (..))
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Default                   (def)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import           ENCOINS.Core.OffChain          (EncoinsMode (..), beaconTx, delegateTx, encoinsSendTx, encoinsTx,
                                                 postEncoinsPolicyTx, postLedgerValidatorTx, stakeOwnerTx)
import           ENCOINS.Core.OnChain           (EncoinsRedeemer, EncoinsRedeemerOnChain, ledgerValidatorAddress,
                                                 minMaxTxOutValueInLedger)
import           Encoins.Relay.Server.Config    (EncoinsRelayConfig (..), loadEncoinsRelayConfig, referenceScriptSalt,
                                                 treasuryWalletAddress)
import           Encoins.Relay.Server.Internal  (EncoinsRelayEnv (..), getEncoinsProtocolParams, getTrackedAddresses)
import           Encoins.Relay.Server.Status    (EncoinsStatusErrors, EncoinsStatusReqBody (MaxAdaWithdraw), EncoinsStatusResult,
                                                 encoinsStatusHandler)
import           Encoins.Relay.Server.Version   (ServerVersion, versionHandler)
import           Encoins.Relay.Verifier.Client  (verifierClient)
import           Encoins.Relay.Verifier.Server  (VerifierApiError (..))
import           GHC.Generics                   (Generic)
import           Ledger                         (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex  (ChainIndex (..), getMapUtxoFromRefs)
import           PlutusAppsExtra.IO.Wallet      (getWalletAddr, getWalletUtxos)
import           PlutusAppsExtra.Types.Tx       (TransactionBuilder, txBuilderRequirements)
import qualified Servant.Client                 as Servant

mkServerHandle :: Config -> IO (ServerHandle EncoinsApi)
mkServerHandle c = do
    EncoinsRelayConfig{..} <- loadEncoinsRelayConfig c
    verifierClientEnv      <- mkServantClientEnv cVerifierPort cVerifierHost cVerifierProtocol
    pure $ ServerHandle
        Kupo
        (EncoinsRelayEnv
            cRefStakeOwner
            cRefBeacon
            cVerifierPkh
            verifierClientEnv
            cDelegationCurrencySymbol
            cDelegationTokenName
            cDelegationServerHost
            cDelegationServerPort
            cDelegationServerProtocol
            cDelegationIp
        )
        getTrackedAddresses
        txBuilders
        (pure ())
        processRequest
        encoinsStatusHandler
        checkStatusEndpoint
        versionHandler

type EncoinsApi = ServerApi
    (InputOfEncoinsApi, TransactionInputs)
    EncoinsTxApiError
    EncoinsStatusReqBody
    EncoinsStatusErrors
    EncoinsStatusResult
    ServerVersion

type instance InputOf        EncoinsApi = InputOfEncoinsApi
type instance AuxillaryEnvOf EncoinsApi = EncoinsRelayEnv

data InputOfEncoinsApi
    = InputRedeemer   EncoinsRedeemer EncoinsMode
    | InputSending    Address CSL.Value Address
    | InputDelegation Address Text
    deriving (Show, Generic, FromJSON, ToJSON)

data EncoinsTxApiError
    = CorruptedExternalInputs
    | CorruptedValue
    | VerifierError VerifierApiError
    deriving (Show, Exception)

instance IsCardanoServerError EncoinsTxApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        CorruptedExternalInputs      -> "The request contained corrupted external transaction inputs data."
        CorruptedValue               -> "The request contained corrupted value data."
        VerifierError IncorrectInput -> "The request contained incorrect public input."
        VerifierError IncorrectProof -> "The request contained incorrect proof."

serverSetup :: ServerM EncoinsApi ()
serverSetup = void $ do
    encoinsProtocolParams@(_, refBeacon, _) <- getEncoinsProtocolParams
    -- Mint the stake owner token
    utxos <- getWalletUtxos mempty
    let utxos' = Map.delete refBeacon utxos
    mkTx [] (InputContextServer utxos') [stakeOwnerTx encoinsProtocolParams]
    -- Mint and send the beacon
    utxos'' <- getWalletUtxos mempty
    mkTx [] (InputContextServer utxos'') [beaconTx encoinsProtocolParams]
    -- Post the ENCOINS minting policy
    mkTx [] def [postEncoinsPolicyTx encoinsProtocolParams referenceScriptSalt]
    -- Post the staking validator policy
    mkTx [] def [postLedgerValidatorTx encoinsProtocolParams referenceScriptSalt]
    mkTx [] def [encoinsSendTx encoinsProtocolParams (ledgerValidatorAddress encoinsProtocolParams) minMaxTxOutValueInLedger]

processRequest :: (InputOf EncoinsApi, TransactionInputs) -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest req = sequence $ case req of
    r@(InputRedeemer ((_, changeAddr, _), _, _, _) mode, _) -> mkContext mode changeAddr <$> r
    s@(InputSending _  _ changeAddr, _)                     -> mkContext WalletMode changeAddr <$> s
    d@(InputDelegation changeAddr _, _)                     -> mkContext WalletMode changeAddr <$> d
    where
        mkContext mode addr inputsCSL = do
            builders <- txBuilders (fst req)
            reqs     <- liftIO $ txBuilderRequirements builders
            case mode of
                WalletMode -> do
                    utxos    <- getMapUtxoFromRefs reqs $ fromMaybe (throw CorruptedExternalInputs) (fromCSL inputsCSL)
                    pure $ InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
                LedgerMode -> do
                    utxos    <- getWalletUtxos reqs
                    InputContextClient mempty utxos (TxOutRef (TxId "") 1) <$> getWalletAddr

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (InputRedeemer red mode) = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    relayWalletAddress <- getWalletAddr
    red' <- verifyRedeemer red
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) encoinsProtocolParams red' mode]

txBuilders (InputSending addr valCSL _) = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    pure [encoinsSendTx encoinsProtocolParams addr $ fromMaybe (throw CorruptedValue) $ fromCSL valCSL]

txBuilders (InputDelegation addr ipAddr) = do
    (cs, tokenName) <- (envDelegationCurrencySymbol &&& envDelegationTokenName) <$> getAuxillaryEnv
    pure [delegateTx cs tokenName addr ipAddr]

verifyRedeemer :: EncoinsRedeemer -> ServerM EncoinsApi EncoinsRedeemerOnChain
verifyRedeemer red = do
    verifierClientEnv <- envVerifierClientEnv <$> getAuxillaryEnv
    let ?servantClientEnv = verifierClientEnv
    liftIO (verifierClient red) >>= either throwM pure

-- Check if status endpoint is alive
checkStatusEndpoint :: ServerM EncoinsApi (Either Text ())
checkStatusEndpoint = do
    env <- mkServerClientEnv
    res <- liftIO $ Servant.runClientM (statusC @EncoinsApi MaxAdaWithdraw) env
    either throwM (const $ pure $ Right ()) res
