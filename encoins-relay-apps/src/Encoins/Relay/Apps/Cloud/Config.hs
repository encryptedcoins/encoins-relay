{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}


module Encoins.Relay.Apps.Cloud.Config where

import           Encoins.Common.Log                         (mkLogEnv,
                                                             withLogEnv)
import           Encoins.Relay.Apps.Cloud.PostgreSQL.Migration (migration)
import           Encoins.Relay.Apps.Cloud.Types

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

withCloudEnv :: (CloudEnv -> IO ()) -> IO ()
withCloudEnv action = do
  config <- decodeOrErrorFromFile "cloud_config.json"
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
          let env = mkCloudEnv manager config le maestroToken pool
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

mkCloudEnv :: Manager
  -> CloudConfig
  -> LogEnv
  -> MaestroToken
  -> P.Pool
  -> CloudEnv
mkCloudEnv manager cloudConfig logEnv maestroToken pool =
  MkCloudEnv
    { envHyperTextProtocol = icHyperTextProtocol cloudConfig
    , envHost              = icHost cloudConfig
    , envPort              = icPort cloudConfig
    , envNetworkId         = icNetworkId cloudConfig
    , envMaestroToken      = maestroToken
    , envCurrencySymbol    = icCurrencySymbol cloudConfig
    , envManager           = manager
    , envLogEnv            = logEnv
    , envKContext          = mempty
    , envKNamespace        = mempty
    , envFormatMessage     = icFormatMessage cloudConfig
    , envPool              = pool
    }
