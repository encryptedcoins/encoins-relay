module Encoins.Relay.Client.Main where

import           Cardano.Server.Config         (decodeOrErrorFromFile, cAuxiliaryEnvFile)
import           Cardano.Server.Client.Client  (runClientWithOpts)
import           Encoins.Relay.Client.Client   (mkClientHandle)
import           Encoins.Relay.Client.Opts     (Options (..), extractCommonOptions, runWithOpts)
import           Encoins.Relay.Server.Config   (cVerifierConfig)
import           Encoins.Relay.Server.Server   (mkServerHandle)
import           Encoins.Relay.Verifier.Server (cBulletproofSetupFilePath)
import           System.Directory              (createDirectoryIfMissing)

runEncoinsClient :: FilePath -> IO ()
runEncoinsClient cardanoServerConfigFp = do
    createDirectoryIfMissing True "secrets"
    c                <- decodeOrErrorFromFile cardanoServerConfigFp
    ec               <- decodeOrErrorFromFile $ cAuxiliaryEnvFile c
    vc               <- decodeOrErrorFromFile $ cVerifierConfig ec
    bulletproofSetup <- decodeOrErrorFromFile $ cBulletproofSetupFilePath vc
    opts             <- runWithOpts
    sh               <- mkServerHandle c
    runClientWithOpts c sh (mkClientHandle bulletproofSetup (optsEncoinsMode opts)) $ extractCommonOptions opts