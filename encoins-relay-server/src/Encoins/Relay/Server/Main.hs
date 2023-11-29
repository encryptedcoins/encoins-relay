{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Config           (Config (cHyperTextProtocol), decodeOrErrorFromFile)
import           Cardano.Server.Internal         (loadEnv, runServerM)
import           Cardano.Server.Main             (runServer)
import           Data.ByteString                 (ByteString)
import           Data.FileEmbed                  (embedFileIfExists)
import           Encoins.Relay.Server.Delegation (distributeRewards)
import           Encoins.Relay.Server.Opts       (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server     (mkServerHandle, serverSetup)
import           Encoins.Relay.Server.Version    (relayVersion, showRelayVersion)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    putStrLn $ showRelayVersion relayVersion
    config           <- decodeOrErrorFromFile cardanoServerConfigFp
    let ?creds    = creds
        ?protocol = cHyperTextProtocol config
    runWithOpts >>= \case
        Run      -> mkServerHandle config >>= runServer config
        Setup    -> mkServerHandle config >>= loadEnv config >>= (`runServerM` serverSetup)
        Reward r -> mkServerHandle config >>= loadEnv config >>= (`runServerM` distributeRewards r)

creds :: Maybe (ByteString, ByteString)
creds = let keyCred  = $(embedFileIfExists "../key.pem")
            certCred = $(embedFileIfExists "../certificate.pem")
        in (,) <$> certCred <*> keyCred