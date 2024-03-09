{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Encoins.Relay.Client.Main where

import           Cardano.Server.Client.Opts   (runWithOpts)
import           Cardano.Server.Config        (decodeOrErrorFromFile, Config (..))
import           Cardano.Server.Internal      (runAppT, mkServantClientEnv, loadEnv)
import           ENCOINS.Bulletproofs         (BulletproofSetup)
import           Encoins.Relay.Client.Opts    (commandParser, runCommand)
import           Encoins.Relay.Server.Server  (embedCreds, loadEncoinsEnv, EncoinsApi)
import           System.Directory             (createDirectoryIfMissing)

runEncoinsClient :: FilePath -> FilePath -> IO ()
runEncoinsClient configFp bulletproofSetupFp = do
    createDirectoryIfMissing True "secrets"
    let ?creds = embedCreds
    c@Config{..}       <- decodeOrErrorFromFile @(Config EncoinsApi) configFp
    encoinsConfig    <- decodeOrErrorFromFile configFp
    bulletproofSetup <- decodeOrErrorFromFile @BulletproofSetup bulletproofSetupFp
    clientEnv        <- mkServantClientEnv cPort cHost cHyperTextProtocol
    auxEnv           <- loadEncoinsEnv encoinsConfig
    env              <- loadEnv c auxEnv
    opts             <- runWithOpts commandParser
    let ?bulletproofSetup = bulletproofSetup
        ?servantClientEnv = clientEnv
    runAppT env $ runCommand opts