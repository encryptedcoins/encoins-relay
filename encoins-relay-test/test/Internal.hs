module Internal where

import           Cardano.Server.Config          (decodeOrErrorFromFile)
import           Cardano.Server.Internal        (Env (envLogger), ServerM, loadEnv, runServerM)
import           Cardano.Server.Utils.Logger    (mutedLogger)
import           Encoins.Relay.Server.Server    (EncoinsApi, mkServerHandle)

runEncoinsServerM :: ServerM EncoinsApi a -> IO a
runEncoinsServerM ma = do
    let cardanoServerConfigFp = "encoins-relay-test/test/configuration/config.json"
    c <- decodeOrErrorFromFile cardanoServerConfigFp
    env <- mkServerHandle c >>= loadEnv cardanoServerConfigFp
    runServerM env {envLogger = mutedLogger} ma