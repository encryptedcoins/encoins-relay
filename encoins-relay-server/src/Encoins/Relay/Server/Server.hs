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
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Encoins.Relay.Server.Server where

import           Control.Concurrent                           (forkIO)
import           Control.Monad                                (void)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Control.Monad.Reader                         (MonadReader (..))
import           Data.Data                                    (Proxy (..))
import           Data.Default                                 (def)
import qualified Data.Map                                     as Map
import           Development.GitRev                           (gitCommitDate, gitHash)
import           ENCOINS.Core.OffChain                        (beaconTx, encoinsSendTx, postEncoinsPolicyTx, postLedgerValidatorTx,
                                                               stakeOwnerTx)
import           ENCOINS.Core.OnChain                         (ledgerValidatorAddress, minMaxTxOutValueInLedger)
import           Encoins.Relay.Common.Diagnostic              (clientDiagnostics, doDiagnostics, providersDiagnostics)
import           Encoins.Relay.Common.Server                  (mkServantClientEnv)
import           Encoins.Relay.Common.Server.Endpoint.Ping    (PingEndpoint, pingEndpointHandler)
import           Encoins.Relay.Common.Server.Endpoint.Version (VersionEndpoint, versionEndpointHandler)
import           Encoins.Relay.Common.Tx                      (mkTx)
import           Encoins.Relay.Common.Tx.Input                (InputContext (..))
import           Encoins.Relay.Server.Config                  (EncoinsApiTF, referenceScriptSalt)
import           Encoins.Relay.Server.Endpoint.NewTx          (NewTxEndpoint, newTxEndpointHandler)
import           Encoins.Relay.Server.Endpoint.ServerTx       (ServerTxEndpoint, checkForCleanUtxos, processQueue, serverTxEndpointHandler)
import           Encoins.Relay.Server.Endpoint.Status         (EncoinsStatusReqBody (MaxAdaWithdraw), StatusEndpoint, statusEndpointHandler)
import           Encoins.Relay.Server.Endpoint.SubmitTx       (SubmitTxEndpoint, submitTxEndpointHandler)
import           Encoins.Relay.Server.Endpoint.Utxos          (UtxosEndpoint, utxosEndpointHandler)
import           Encoins.Relay.Server.Internal                (EncoinsRelayEnv (..), EncoinsServerM, getEncoinsProtocolParams,
                                                               runEncoinsServerM)
import           Ledger                                       (TxId (TxId), TxOutRef (..))
import           Paths_encoins_relay_server                   (version)
import           PlutusAppsExtra.IO.Wallet                    (getWalletAddr, getWalletUtxos)
import           Servant                                      (HasServer (ServerT), (:<|>) (..))
import           Servant.Client                               (client)

type EncoinsApi
    =    PingEndpoint
    :<|> UtxosEndpoint
    :<|> NewTxEndpoint
    :<|> ServerTxEndpoint
    :<|> SubmitTxEndpoint
    :<|> StatusEndpoint
    :<|> VersionEndpoint

type instance EncoinsApiTF = EncoinsApi

encoinsServer :: ServerT EncoinsApi EncoinsServerM
encoinsServer
    =    pingEndpointHandler
    :<|> utxosEndpointHandler
    :<|> newTxEndpointHandler
    :<|> serverTxEndpointHandler
    :<|> submitTxEndpointHandler
    :<|> statusEndpointHandler
    :<|> versionEndpointHandler version $(gitHash) $(gitCommitDate)

beforeMainLoop :: EncoinsServerM ()
beforeMainLoop = do
    env <- ask
    liftIO $ forkIO $ processQueue env
    liftIO $ forkIO $ diagnotics env
    checkForCleanUtxos
  where
    diagnotics env =  runEncoinsServerM env $ doDiagnostics (fromEnum $ envDiagnosticsInterval env) $ do
        clientEnv <- mkServantClientEnv (envPort env) (envHost env) (envHyperTextProtocol env)
        providersDiagnostics
        clientDiagnostics clientEnv $ client (Proxy @StatusEndpoint) MaxAdaWithdraw

serverSetup :: EncoinsServerM ()
serverSetup = void $ do
    encoinsProtocolParams@(_, refBeacon, _, _) <- getEncoinsProtocolParams
    addr <- getWalletAddr
    -- Mint the stake owner token
    utxos <- getWalletUtxos mempty
    let utxos' = Map.delete refBeacon utxos
    mkTx [] (InputContextClient utxos' utxos' (TxOutRef (TxId "") 1) addr) [stakeOwnerTx encoinsProtocolParams] Nothing
    -- Mint and send the beacon
    utxos'' <-  getWalletUtxos mempty
    mkTx [] (InputContextClient utxos'' utxos'' (TxOutRef (TxId "") 1) addr) [beaconTx encoinsProtocolParams] Nothing
    -- Post the ENCOINS minting policy
    mkTx [] (InputContextServer def) [postEncoinsPolicyTx encoinsProtocolParams referenceScriptSalt] Nothing
    -- Post the staking validator policy
    mkTx [] (InputContextServer def) [postLedgerValidatorTx encoinsProtocolParams referenceScriptSalt] Nothing
    let setupSendTx = encoinsSendTx encoinsProtocolParams (ledgerValidatorAddress encoinsProtocolParams) minMaxTxOutValueInLedger
    mkTx [] (InputContextServer def) [setupSendTx] Nothing
