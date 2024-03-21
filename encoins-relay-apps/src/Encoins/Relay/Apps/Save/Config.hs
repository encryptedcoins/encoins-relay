{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}


module Encoins.Relay.Apps.Save.Config where

import           Encoins.Common.Log                         (mkLogEnv,
                                                             withLogEnv)
import           Encoins.Relay.Apps.Save.Database.Migration (migration)
import           Encoins.Relay.Apps.Save.Types

import           Cardano.Server.Config                      (decodeOrErrorFromFile)
import           Control.Exception.Safe                     (bracket)
import           Data.Time                                  (DiffTime)
import qualified Hasql.Connection                           as Conn
import qualified Hasql.Pool                                 as P
import           Katip
import           Network.HTTP.Client                        hiding (Proxy)
import           Network.HTTP.Client.TLS
import           PlutusAppsExtra.Api.Maestro                (MaestroToken)
import           Text.Pretty.Simple                         (pPrint)

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
    let connSettings = Conn.settings "127.0.0.1" 5432 "postgres" "" "encoins"
    withDefaultPool connSettings $ \pool -> do
      res <- migration pool
      case res of
        Left err -> pPrint err
        Right (Right (Just err)) -> pPrint err
        _ -> do
          manager <- newManager tlsManagerSettings
          let env = mkSaveEnv manager config le maestroToken pool
          action env

withPool :: Int
  -> DiffTime
  -> DiffTime
  -> Conn.Settings
  -> (P.Pool -> IO ())
  -> IO ()
withPool poolSize acqTimeout maxLifetime connSettings = bracket
  ( P.acquire poolSize acqTimeout maxLifetime connSettings )
  P.release

withDefaultPool :: Conn.Settings -> (P.Pool -> IO ()) -> IO ()
withDefaultPool connSettings = withPool 3 10 1_800 connSettings

mkSaveEnv :: Manager
  -> SaveConfig
  -> LogEnv
  -> MaestroToken
  -> P.Pool
  -> SaveEnv
mkSaveEnv manager saveConfig logEnv maestroToken pool =
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
    , envPool              = pool
    }
