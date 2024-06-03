{-# LANGUAGE ImplicitParams #-}

module Internal where

import           Encoins.Relay.Common          (decodeOrErrorFromFile)
import           Encoins.Relay.Server.Internal (EncoinsServerM, loadEncoinsRelayEnv)
import qualified Encoins.Relay.Server.Internal as EncoinsServer
import           Encoins.Relay.Server.Server   (EncoinsApi)

runEncoinsServerM :: EncoinsServerM a -> IO a
runEncoinsServerM ma = do
    let cardanoServerConfigFp = "encoins-relay-test/test/configuration/relayConfig.json"
    c <- decodeOrErrorFromFile cardanoServerConfigFp
    env <- loadEncoinsRelayEnv c
    EncoinsServer.runEncoinsServerM env ma
