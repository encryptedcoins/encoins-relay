{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Cardano.Api                         (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import qualified Control.Concurrent                  as C
import           Control.Exception                   (bracket)
import qualified Data.Map                            as Map
import           Data.Maybe                          (listToMaybe)
import           Encoins.Relay.Common                (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Client         (HasServantClientEnv)
import           Encoins.Relay.Common.Server         (mkServantClientEnv)
import           Encoins.Relay.Delegation.Client     (DelegationClientError (DelegationServerError), currentServersClient,
                                                      serverDelegatesClient, serversClient)
import           Encoins.Relay.Delegation.Internal   (DelegationConfig (..))
import           Encoins.Relay.Delegation.Server     (DelegationServerError (..), runDelegationServer)
import qualified Encoins.Relay.Delegation.ServerSpec as Server
import           System.Directory                    (setCurrentDirectory)
import           Test.Hspec                          (Expectation, Spec, context, describe, hspec, it, runIO, shouldBe)
import           Test.QuickCheck                     (Property, Testable (property))

main :: IO ()
main = do
    setCurrentDirectory "encoins-relay-delegation/test"
    let delegConfigFp = "configuration/delegConfig.json"
    delegConfig <- decodeOrErrorFromFile delegConfigFp
    delegClientEnv <- mkServantClientEnv
        (cPort delegConfig)
        (cHost delegConfig)
        (cHyperTextProtocol delegConfig)
    bracket
        (do
            C.forkIO $ runDelegationServer delegConfigFp
        )
        C.killThread
        $ const $ hspec $ do
            runIO $ C.threadDelay 90_000_000
            let ?servantClientEnv = delegClientEnv
            Server.spec
