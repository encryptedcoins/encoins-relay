{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.Server where

import           CSL                                      (TransactionUnspentOutputs)
import           CSL.Class                                (FromCSL (..))
import           Cardano.Server.Config                    (Config (..), decodeOrErrorFromFile, loadConfig)
import           Cardano.Server.Error                     (IsCardanoServerError (errMsg, errStatus), toEnvelope)
import           Cardano.Server.Input                     (InputContext (..))
import           Cardano.Server.Internal                  (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (..), ServerM,
                                                           getAuxillaryEnv)
import           Cardano.Server.Main                      (ServerApi)
import           Cardano.Server.Tx                        (mkTx)
import           Control.Arrow                            ((&&&))
import           Control.Exception                        (Exception, throw)
import           Control.Monad                            (void)
import           Control.Monad.Catch                      (MonadThrow (..))
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Default                             (def)
import qualified Data.Map                                 as Map
import           ENCOINS.Core.OffChain                    (beaconTx, encoinsTx, postEncoinsPolicyTx, postStakingValidatorTx,
                                                           stakeOwnerTx)
import           ENCOINS.Core.OnChain                     (EncoinsRedeemer, beaconCurrencySymbol, encoinsSymbol,
                                                           ledgerValidatorAddress)
import           ENCOINS.Core.V1.OffChain                 (EncoinsMode (..))
import           Encoins.Relay.Server.Config              (EncoinsRelayConfig (..), referenceScriptSalt, treasuryWalletAddress,
                                                           verifierPKH)
import           Encoins.Relay.Verifier.Client            (mkVerifierClientEnv, verifierClient)
import           Encoins.Relay.Verifier.Server            (VerifierApiError (..))
import           Ledger                                   (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet                (getWalletAddr, getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder)
import           Servant.Client                           (ClientEnv (..))
import           Text.Hex                                 (Text)

mkServerHandle :: IO (ServerHandle EncoinsApi)
mkServerHandle = do
    EncoinsRelayConfig{..} <- loadConfig >>= decodeOrErrorFromFile . cAuxiliaryEnvFile
    verifierClientEnv <- decodeOrErrorFromFile cVerifierConfig >>= mkVerifierClientEnv
    pure $ ServerHandle
        Kupo
        (EncoinsRelayEnv cRefStakeOwner cRefBeacon verifierClientEnv)
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
    (refStakeOwner, refBeacon) <- getRefs
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

processRequest :: (InputOf EncoinsApi, TransactionUnspentOutputs) -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest ((red@((_, addr), _, _, _), mode), utxosCSL) = do
    let utxos   = maybe (throw CorruptedExternalUTXOs) Map.fromList $ fromCSL utxosCSL
        context = InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
    return ((red, mode), context)

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (red, mode) = do
    (refStakeOwner, refBeacon) <- getRefs
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

----------------------------------------------------------------- Env ---------------------------------------------------------------------

data EncoinsRelayEnv = EncoinsRelayEnv
    { envRefStakeOwner     :: TxOutRef
    , envRefBeacon         :: TxOutRef
    , envVerifierClientEnv :: ClientEnv
    }

getRefs :: ServerM EncoinsApi (TxOutRef, TxOutRef)
getRefs = (envRefStakeOwner &&& envRefBeacon) <$> getAuxillaryEnv

getTrackedAddresses :: ServerM EncoinsApi [Address]
getTrackedAddresses = do
    (refStakeOwner, refBeacon) <- getRefs
    let encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
        stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
    return [ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb), alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: ServerM EncoinsApi Address
getLedgerAddress = head <$> getTrackedAddresses