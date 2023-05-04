module Internal where

import           Cardano.Server.Internal        (Env (envLogger), ServerM, loadEnv, runServerM)
import           Cardano.Server.Utils.Logger    (mutedLogger)
import           Encoins.Relay.Server.Server    (EncoinsApi, mkServerHandle)

runEncoinsServerM :: ServerM EncoinsApi a -> IO a
runEncoinsServerM ma = do
    env <- mkServerHandle >>= loadEnv
    runServerM env {envLogger = mutedLogger} ma