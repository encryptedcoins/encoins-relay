{-# LANGUAGE ImplicitParams #-}

module Internal where

import           Cardano.Server.Config          (decodeOrErrorFromFile)
import           Cardano.Server.Internal        (Env (envLogger), ServerM, loadEnv, runServerM)
import           Cardano.Server.Utils.Logger    (mutedLogger)
import           Encoins.Relay.Server.Server    (EncoinsApi, embedCreds, loadEncoinsEnv)

runEncoinsServerM :: ServerM EncoinsApi a -> IO a
runEncoinsServerM ma = do
    let cardanoServerConfigFp = "encoins-relay-test/test/configuration/config.json"
    let ?creds = embedCreds
    config <- decodeOrErrorFromFile cardanoServerConfigFp
    auxEnv <- loadEncoinsEnv config
    env <- loadEnv config auxEnv
    runServerM env {envLogger = mutedLogger} ma