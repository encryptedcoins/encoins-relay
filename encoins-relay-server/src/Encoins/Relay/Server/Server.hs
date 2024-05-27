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
import           Data.Default                                 (def)
import qualified Data.Map                                     as Map
import           Development.GitRev                           (gitCommitDate, gitHash)
import           ENCOINS.Core.OffChain                        (beaconTx, encoinsSendTx, postEncoinsPolicyTx, postLedgerValidatorTx,
                                                               stakeOwnerTx)
import           ENCOINS.Core.OnChain                         (ledgerValidatorAddress, minMaxTxOutValueInLedger)
import           Encoins.Relay.Common.Server.Endpoint.Ping    (PingEndpoint, pingEndpointHandler)
import           Encoins.Relay.Common.Server.Endpoint.Version (VersionEndpoint, versionEndpointHandler)
import           Encoins.Relay.Common.Tx                      (mkTx)
import           Encoins.Relay.Common.Tx.Input                (InputContext (..))
import           Encoins.Relay.Server.Config                  (referenceScriptSalt, EncoinsApiTF)
import           Encoins.Relay.Server.Endpoint.NewTx          (NewTxEndpoint, newTxEndpointHandler)
import           Encoins.Relay.Server.Endpoint.ServerTx       (ServerTxEndpoint, checkForCleanUtxos, processQueue, serverTxEndpointHandler)
import           Encoins.Relay.Server.Endpoint.Status         (StatusEndpoint, statusEndpointHandler)
import           Encoins.Relay.Server.Endpoint.SubmitTx       (SubmitTxEndpoint, submitTxEndpointHandler)
import           Encoins.Relay.Server.Endpoint.Utxos          (UtxosEndpoint, utxosEndpointHandler)
import           Encoins.Relay.Server.Internal                (EncoinsServerM, getEncoinsProtocolParams)
import           Ledger                                       (TxId (TxId), TxOutRef (..))
import           Paths_encoins_relay_server                   (version)
import           PlutusAppsExtra.IO.Wallet                    (getWalletAddr, getWalletUtxos)
import           Servant                                      (HasServer (ServerT), (:<|>) (..))

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
    checkForCleanUtxos

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
