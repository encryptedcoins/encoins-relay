{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Encoins.Relay.Server.Server where

import           Cardano.Node.Emulator                    (Params (..), pParamsFromProtocolParams)
import           Cardano.Server.Config                    (Config (..), Creds, decodeOrErrorFromFile, withDefault, AuxillaryConfigOf)
import           Cardano.Server.Endpoints.Ping            (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Submit          (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Endpoints.Utxos           (UtxosApi, utxosHandler)
import           Cardano.Server.Endpoints.Version         (VersionApi)
import           Cardano.Server.Input                     (InputContext (..))
import           Cardano.Server.Internal                  (AuxillaryEnvOf, ServerM, mkServantClientEnv, mkServerClientEnv, AppT)
import           Cardano.Server.Tx                        (mkTx)
import           Control.Lens                             ((^?))
import           Control.Monad                            (void)
import           Control.Monad.Catch                      (MonadThrow (..))
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Aeson                               (fromJSON)
import qualified Data.Aeson                               as J
import           Data.Aeson.Lens                          (key)
import           Data.Data                                (Proxy (..))
import           Data.Default                             (def)
import           Data.FileEmbed                           (embedFileIfExists)
import           Data.Functor                             ((<&>))
import qualified Data.Map                                 as Map
import           Data.Text                                (Text)
import           ENCOINS.Core.OffChain                    (beaconTx, encoinsSendTx, postEncoinsPolicyTx, postLedgerValidatorTx,
                                                           stakeOwnerTx)
import           ENCOINS.Core.OnChain                     (ledgerValidatorAddress, minMaxTxOutValueInLedger)
import           Encoins.Relay.Server.Config              (EncoinsRelayConfig (..), referenceScriptSalt)
import           Encoins.Relay.Server.Endpoints.Tx.New    (NewTxApi, newTxHandler)
import           Encoins.Relay.Server.Endpoints.Tx.Server (QueueRef, ServerTxApi, serverTxHandler)
import           Encoins.Relay.Server.Internal            (EncoinsRelayEnv (..), getEncoinsProtocolParams)
import           Encoins.Relay.Server.Status              (StatusApi, encoinsStatusHandler)
import           Encoins.Relay.Server.Version             (versionHandler)
import           Ledger                                   (TxId (TxId), TxOutRef (..))
import qualified PlutusAppsExtra.IO.ChainIndex            as ChainIndex
import qualified PlutusAppsExtra.IO.Tx                    as Tx
import           PlutusAppsExtra.IO.Wallet                (getWalletAddr, getWalletUtxos)
import qualified PlutusAppsExtra.IO.Wallet                as Wallet
import           Servant                                  (type (:<|>) ((:<|>)))
import           Servant.Client                           (client)
import qualified Servant.Client                           as Servant
import           Servant.Server                           (HasServer (..), ServerT)
import           System.Random                            (randomIO)

encoinsServer :: FilePath -> QueueRef -> ServerT EncoinsApi (ServerM EncoinsApi)
encoinsServer nodeFilePath queueRef
    = pingHandler
    :<|> versionHandler
    :<|> utxosHandler
    :<|> newTxHandler
    :<|> serverTxHandler queueRef
    :<|> submitTxHandler nodeFilePath
    :<|> encoinsStatusHandler

loadEncoinsEnv :: Config EncoinsApi -> IO EncoinsRelayEnv
loadEncoinsEnv Config{cAuxilaryConfig = EncoinsRelayConfig{..}} = do
    let ?creds = embedCreds
    envVerifierClientEnv  <- mkServantClientEnv cVerifierPort cVerifierHost cVerifierProtocol
    pp <- decodeOrErrorFromFile cProtocolParametersFile
    slotConfig <- do
        val <- decodeOrErrorFromFile @J.Value cSlotConfigFile
        case val ^? key "cicSlotConfig" <&> fromJSON of
            Just (J.Success sc) -> pure sc
            _                   -> error "There is no slot config in chain index config file."
    envWallet <- sequence $ decodeOrErrorFromFile <$> cWalletFile
    envBlockfrostToken <- sequence $ decodeOrErrorFromFile <$> cBlockfrostTokenFilePath
    envMaestroToken <- sequence $ decodeOrErrorFromFile <$> cMaestroTokenFilePath
    envWalletProvider <- withDefault Wallet.Cardano cWalletProvider
    envChainIndexProvider <- withDefault ChainIndex.Kupo cChainIndexProvider
    envTxProvider <- withDefault Tx.Cardano cTxProvider
    envDiagnosticsInterval <- withDefault 1800 cDiagnosticsInteval
    pure $ EncoinsRelayEnv
        { envRefStakeOwner            = cRefStakeOwner
        , envRefBeacon                = cRefBeacon
        , envVerifierPKH              = cVerifierPkh
        , envDelegationCurrencySymbol = cDelegationCurrencySymbol
        , envDelegationTokenName      = cDelegationTokenName
        , envDelegationSeverHost      = cDelegationServerHost
        , envDelegationServerPort     = cDelegationServerPort
        , envDelegationServerProtocol = cDelegationServerProtocol
        , envDelegationIp             = cDelegationIp
        , envCollateral               = cCollateral
        , envProtocolParams           = Params slotConfig (pParamsFromProtocolParams pp) cNetworkId
        , envMinUtxosNumber           = cMaxUtxosNumber
        , envMaxUtxosNumber           = cMinUtxosNumber
        , envNetworkId                = cNetworkId
        , ..
        }

-- Embed https cert and key files on compilation
embedCreds :: Creds
embedCreds =
    let keyCred  = $(embedFileIfExists "../key.pem" )
        certCred = $(embedFileIfExists "../certificate.pem")
    in (,) <$> certCred <*> keyCred

type EncoinsApi
    =    PingApi
    :<|> VersionApi
    :<|> UtxosApi
    :<|> NewTxApi
    :<|> ServerTxApi
    :<|> SubmitTxApi
    :<|> StatusApi

type instance AuxillaryConfigOf EncoinsApi = EncoinsRelayConfig
type instance AuxillaryEnvOf    EncoinsApi = EncoinsRelayEnv

serverSetup :: ServerM EncoinsApi ()
serverSetup = void $ do
    encoinsProtocolParams@(_, refBeacon, _) <- getEncoinsProtocolParams
    addr <- getWalletAddr
    -- Mint the stake owner token
    utxos <- getWalletUtxos mempty
    let utxos' = Map.delete refBeacon utxos
    mkTx [] (InputContextClient utxos' utxos' (TxOutRef (TxId "") 1) addr) [stakeOwnerTx encoinsProtocolParams]
    -- Mint and send the beacon
    utxos'' <-  getWalletUtxos mempty
    mkTx [] (InputContextClient utxos'' utxos'' (TxOutRef (TxId "") 1) addr) [beaconTx encoinsProtocolParams]
    -- Post the ENCOINS minting policy
    mkTx [] (InputContextServer def) [postEncoinsPolicyTx encoinsProtocolParams referenceScriptSalt]
    -- Post the staking validator policy
    mkTx [] (InputContextServer def) [postLedgerValidatorTx encoinsProtocolParams referenceScriptSalt]
    mkTx [] (InputContextServer def) [encoinsSendTx encoinsProtocolParams (ledgerValidatorAddress encoinsProtocolParams) minMaxTxOutValueInLedger]

-- Check if status endpoint is alive
checkStatusEndpoint :: (MonadThrow m, MonadIO m) => AppT EncoinsApi m (Either Text ())
checkStatusEndpoint = do
    env <- mkServerClientEnv
    req <- randomIO
    res <- liftIO $ Servant.runClientM (client (Proxy @StatusApi) req) env
    either throwM (const $ pure $ Right ()) res
