module Main where

import           Cardano.Server.Config         (decodeOrErrorFromFile, cAuxiliaryEnvFile)
import           Cardano.Server.Client.Client  (runClientWithOpts)
import           Encoins.Relay.Client.Client   (mkClientHandle)
import           Encoins.Relay.Client.Opts     (Options (..), extractCommonOptions, runWithOpts)
import           Encoins.Relay.Server.Config   (cVerifierConfig)
import           Encoins.Relay.Server.Server   (mkServerHandle)
import           Encoins.Relay.Verifier.Server (cBulletproofSetupFilePath)
import           System.Directory              (createDirectoryIfMissing)

main :: IO ()
main =  do
    createDirectoryIfMissing True "secrets"
    c                <- decodeOrErrorFromFile "config.json"
    bulletproofSetup <- decodeOrErrorFromFile "../verifier/bulletproof_setup.json"
    opts             <- runWithOpts
    sh               <- mkServerHandle c
    runClientWithOpts c sh (mkClientHandle bulletproofSetup (optsEncoinsMode opts)) $ extractCommonOptions opts