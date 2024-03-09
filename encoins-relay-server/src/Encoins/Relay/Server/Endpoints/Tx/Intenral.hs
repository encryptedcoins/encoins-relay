{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.Endpoints.Tx.Intenral where

import           CSL                           (TransactionInputs)
import qualified CSL
import           CSL.Class                     (FromCSL (..))
import           Cardano.Server.Error          (IsCardanoServerError (errMsg, errStatus))
import           Cardano.Server.Input          (InputContext (..))
import           Cardano.Server.Internal       (AuxillaryEnvOf, ServerM, getAuxillaryEnv)
import           Cardano.Server.Utils.Logger   ((.<))
import           Control.Arrow                 ((&&&))
import           Control.Exception             (Exception, throw)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           ENCOINS.Core.OffChain         (EncoinsMode (..), delegateTx, encoinsSendTx, encoinsTx)
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Relay.Server.Config   (treasuryWalletAddress)
import           Encoins.Relay.Server.Internal (EncoinsRelayEnv (..), getEncoinsProtocolParams)
import           Encoins.Relay.Verifier.Client (VerifierClientError (..), verifierClient)
import           Encoins.Relay.Verifier.Server (VerifierApiError (..))
import           GHC.Generics                  (Generic)
import           Ledger                        (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex (getMapUtxoFromRefs)
import           PlutusAppsExtra.IO.Wallet     (getWalletAddr, getWalletUtxos)
import           PlutusAppsExtra.Types.Tx      (TransactionBuilder, txBuilderRequirements)
import qualified Servant.Client                as Servant

data InputOfEncoinsApi
    = InputRedeemer   EncoinsRedeemer EncoinsMode
    | InputSending    Address CSL.Value Address
    | InputDelegation Address Text
    deriving (Show, Generic, FromJSON, ToJSON)

type InputWithContext = (InputOfEncoinsApi, InputContext)

data EncoinsTxApiError
    = CorruptedExternalInputs
    | CorruptedValue
    | VerifierError VerifierApiError
    | UnavailableVerifier Servant.ClientError
    deriving (Show, Exception)

instance IsCardanoServerError EncoinsTxApiError where
    errStatus = \case
        UnavailableVerifier{} -> toEnum 500
        _                     -> toEnum 422
    errMsg = \case
        CorruptedExternalInputs      -> "The request contained corrupted external transaction inputs data."
        CorruptedValue               -> "The request contained corrupted value data."
        VerifierError IncorrectInput -> "The request contained incorrect public input."
        VerifierError IncorrectProof -> "The request contained incorrect proof."
        UnavailableVerifier err      -> "The verifier is unavailable.\nDetails:" .< err


processRequest :: (AuxillaryEnvOf api ~ EncoinsRelayEnv) => (InputOfEncoinsApi, TransactionInputs) -> ServerM api InputWithContext
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

txBuilders :: AuxillaryEnvOf api ~ EncoinsRelayEnv => InputOfEncoinsApi -> ServerM api [TransactionBuilder ()]
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

verifyRedeemer :: AuxillaryEnvOf api ~ EncoinsRelayEnv => EncoinsRedeemer -> ServerM api EncoinsRedeemerOnChain
verifyRedeemer red = do
        verifierClientEnv <- envVerifierClientEnv <$> getAuxillaryEnv
        let ?servantClientEnv = verifierClientEnv
        liftIO (verifierClient red) >>= either rethrow pure
    where
        rethrow = \case
            VerifierApiError    verifierErr -> throw $ VerifierError verifierErr
            VerifierClientError clientErr   -> throw $ UnavailableVerifier clientErr