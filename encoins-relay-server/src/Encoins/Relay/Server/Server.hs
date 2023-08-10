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

import           CSL                                  (TransactionInputs)
import qualified CSL
import           CSL.Class                            (FromCSL (..))
import           Cardano.Server.Config                (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Error                 (IsCardanoServerError (errMsg, errStatus))
import           Cardano.Server.Input                 (InputContext (..))
import           Cardano.Server.Internal              (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (..), ServerM,
                                                       getAuxillaryEnv)
import           Cardano.Server.Main                  (ServerApi)
import           Cardano.Server.Tx                    (mkTx)
import           Control.Exception                    (Exception, throw)
import           Control.Monad                        (void)
import           Control.Monad.Catch                  (MonadThrow (..))
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Default                         (def)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           ENCOINS.Core.OffChain                (EncoinsMode(..), beaconTx, encoinsTx, postEncoinsPolicyTx, postLedgerValidatorTx,
                                                        stakeOwnerTx, encoinsSendTx )
import           ENCOINS.Core.OnChain                 (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Relay.Server.Config          (EncoinsRelayConfig (..), referenceScriptSalt, treasuryWalletAddress,
                                                       loadEncoinsRelayConfig)
import           Encoins.Relay.Server.Internal        (EncoinsRelayEnv (EncoinsRelayEnv, envVerifierClientEnv),
                                                       getEncoinsProtocolParams, getTrackedAddresses)
import           Encoins.Relay.Server.Status          (EncoinsStatusErrors, EncoinsStatusReqBody, EncoinsStatusResult,
                                                       encoinsStatusHandler)
import           Encoins.Relay.Verifier.Client        (mkVerifierClientEnv, verifierClient)
import           Encoins.Relay.Verifier.Server        (VerifierApiError (..), VerifierConfig (..))
import           Ledger                               (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex        (ChainIndex (..), getMapUtxoFromRefs)
import           PlutusAppsExtra.IO.Wallet            (getWalletAddr, getWalletUtxos)
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder)

mkServerHandle :: Config -> IO (ServerHandle EncoinsApi)
mkServerHandle c = do
    EncoinsRelayConfig{..} <- loadEncoinsRelayConfig c
    verifierClientEnv      <- mkVerifierClientEnv cVerifierConfig
    verifierPKH            <- cVerifierPkh <$> decodeOrErrorFromFile cVerifierConfig
    pure $ ServerHandle
        Kupo
        (EncoinsRelayEnv cRefStakeOwner cRefBeacon verifierPKH verifierClientEnv)
        getTrackedAddresses
        txBuilders
        (pure ())
        processRequest
        encoinsStatusHandler

type EncoinsApi = ServerApi
    (Either (Address, CSL.Value, Address) (EncoinsRedeemer, EncoinsMode), TransactionInputs)
    EncoinsTxApiError
    EncoinsStatusReqBody
    EncoinsStatusErrors
    EncoinsStatusResult

type instance InputOf        EncoinsApi = Either (Address, CSL.Value, Address) (EncoinsRedeemer, EncoinsMode)
type instance AuxillaryEnvOf EncoinsApi = EncoinsRelayEnv

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

processRequest :: (InputOf EncoinsApi, TransactionInputs) -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest req = sequence $ case req of
    r@(Right (((_, changeAddr, _), _, _, _), mode), _) -> mkContext mode changeAddr <$> r
    l@(Left (_, _, changeAddr), _)                     -> mkContext WalletMode changeAddr <$> l
    where
        mkContext WalletMode addr inputsCSL  = do
            utxos <- getMapUtxoFromRefs $ fromMaybe (throw CorruptedExternalInputs) (fromCSL inputsCSL)
            pure $ InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
        mkContext LedgerMode _ _  = do
            utxos <- getWalletUtxos
            InputContextClient mempty utxos (TxOutRef (TxId "") 1) <$> getWalletAddr

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (Right (red, mode)) = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    relayWalletAddress <- getWalletAddr
    red' <- verifyRedeemer red
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) encoinsProtocolParams red' mode]
txBuilders (Left (addr, valCSL, _)) = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    pure [encoinsSendTx encoinsProtocolParams addr $ fromMaybe (throw CorruptedValue) $ fromCSL valCSL]

verifyRedeemer :: EncoinsRedeemer -> ServerM EncoinsApi EncoinsRedeemerOnChain
verifyRedeemer red = do
    verifierClientEnv <- envVerifierClientEnv <$> getAuxillaryEnv
    let ?servantClientEnv = verifierClientEnv
    liftIO (verifierClient red) >>= either throwM pure