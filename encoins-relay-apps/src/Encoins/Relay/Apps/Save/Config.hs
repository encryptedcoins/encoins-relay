{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Apps.Save.Config where

import           Encoins.Common.Log            (mkLogEnv, withLogEnv)
import           Encoins.Relay.Apps.Save.Types

import           Cardano.Server.Config         (decodeOrErrorFromFile)
import qualified Hasql.Connection              as Connection
import           Katip
import           Network.HTTP.Client           hiding (Proxy)
import           Network.HTTP.Client.TLS
import           PlutusAppsExtra.Api.Maestro   (MaestroToken)
import           Text.Pretty.Simple            (pPrint)

withSaveEnv :: (SaveEnv -> IO ()) -> IO ()
withSaveEnv action = do
  config <- decodeOrErrorFromFile "save_config.json"
  maestroToken <- decodeOrErrorFromFile $ icMaestroTokenFilePath config
  pPrint config
  let logEnv = mkLogEnv
        "Save"
        (icEnvironment config)
        (icVerbosity config)
        (icSeverity config)
  withLogEnv logEnv $ \le -> do
    manager <- newManager tlsManagerSettings
    let connSettings = Connection.settings "127.0.0.1" 5432 "postgres" "" "encoins"
    let env = mkSaveEnv manager config le maestroToken connSettings
    action env

mkSaveEnv :: Manager
  -> SaveConfig
  -> LogEnv
  -> MaestroToken
  -> Connection.Settings
  -> SaveEnv
mkSaveEnv manager saveConfig logEnv maestroToken connSettings =
  MkIpfsEnv
    { envHyperTextProtocol = icHyperTextProtocol saveConfig
    , envHost              = icHost saveConfig
    , envPort              = icPort saveConfig
    , envNetworkId         = icNetworkId saveConfig
    , envMaestroToken      = maestroToken
    , envCurrencySymbol    = icCurrencySymbol saveConfig
    , envManager           = manager
    , envLogEnv            = logEnv
    , envKContext          = mempty
    , envKNamespace        = mempty
    , envFormatMessage     = icFormatMessage saveConfig
    , envConnection        = connSettings
    }
