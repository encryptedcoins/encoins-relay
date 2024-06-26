{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}


module Encoins.Relay.Cloud.Config where

import           Encoins.Common.Log                       (mkLogEnv, withLogEnv)
import           Encoins.Relay.Cloud.PostgreSQL.Migration (migration)
import           Encoins.Relay.Cloud.PostgreSQL.Query     (countEncoinsRowsS)
import           Encoins.Relay.Cloud.Types
import           Encoins.Relay.Common                     (decodeOrErrorFromFile)

import           Control.Exception.Safe                   (bracket)
import           Data.Time                                (DiffTime)
import           Dhall                                    (auto, input)
import qualified Hasql.Connection                         as Conn
import qualified Hasql.Pool                               as P
import qualified Hasql.Pool.Config                        as P
import           Katip
import           Network.HTTP.Client                      hiding (Proxy)
import           Network.HTTP.Client.TLS
import           PlutusAppsExtra.Api.Maestro              (MaestroToken)
import           Text.Pretty.Simple                       (pPrint, pPrintString)

withCloudEnv :: (CloudEnv -> IO ()) -> IO ()
withCloudEnv action = do
  -- config <- decodeOrErrorFromFile "cloud_config.json"
  config <- input auto "./cloud_config.dhall"

  maestroToken <- decodeOrErrorFromFile $ icMaestroTokenFilePath config
  pPrint config
  let logEnv = mkLogEnv
        "Cloud"
        (icEnvironment config)
        (icVerbosity config)
        (icSeverity config)
  withLogEnv logEnv $ \le -> do
    let connSettings = Conn.settings "127.0.0.1" 5432 "postgres" "" "encoins"
    withDefaultPool connSettings $ \pool -> do
      migration pool migrationDir
      rowNumber <- P.use pool countEncoinsRowsS
      pPrintString $ "Number of rows in encoins table: " <> show rowNumber
      manager <- newManager tlsManagerSettings
      let env = mkCloudEnv manager config le maestroToken pool
      action env

migrationDir :: FilePath
migrationDir = "schema"

withPool :: Int
  -> DiffTime
  -> DiffTime
  -> Conn.Settings
  -> (P.Pool -> IO ())
  -> IO ()
withPool poolSize acqTimeout maxLifetime connSettings =
  let conf = P.settings
        [ P.size poolSize
        , P.acquisitionTimeout acqTimeout
        , P.agingTimeout maxLifetime
        , P.staticConnectionSettings connSettings
        ]
  in bracket
  ( P.acquire conf )
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
    , envDiscardPeriod     = icDiscardPeriod cloudConfig
    , envCleanDelay        = icCleanDelay cloudConfig
    }
