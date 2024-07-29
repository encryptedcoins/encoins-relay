{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Delegation.ServerSpec where

import           Cardano.Api                     (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import qualified Control.Concurrent              as C
import           Control.Exception               (bracket)
import qualified Data.Map                        as Map
import           Data.Maybe                      (listToMaybe)
import           Encoins.Relay.Common.Client     (HasServantClientEnv)
import           Encoins.Relay.Delegation.Client (DelegationClientError (DelegationServerError), currentServersClient,
                                                  serverDelegatesClient, serversClient)
import           Encoins.Relay.Delegation.Server (DelegationServerError (..), runDelegationServer)
import           System.Directory                (setCurrentDirectory)
import           Test.Hspec                      (Expectation, Spec, context, describe, hspec, it, runIO, shouldBe)
import           Test.QuickCheck                 (Property, Testable (property))

spec :: HasServantClientEnv => Spec
spec = describe "Delegation server" $ do

    context "/servers" $ do
        it "gets list of servers" propServers

    context "/current" $ do
        it "gets only servers with number of delegated tokens more than number, specified in config" propCurrent

    context "/delegates" $ do
        it "gets map with pkhs and balances of the delegates when all is ok" propDelegatesOk
        it "thows 404 to unknown ips" propDelegates404

propServers :: HasServantClientEnv => Expectation
propServers = serversClient >>= (`shouldBe` Right (Map.fromList
    [("localhost",4000),("localhost:3000",8000)]))

propCurrent :: HasServantClientEnv => Expectation
propCurrent = currentServersClient >>= (`shouldBe` Right ["localhost", "localhost:3000"])

propDelegatesOk :: HasServantClientEnv => Expectation
propDelegatesOk = serverDelegatesClient "localhost:3000" >>=
    (`shouldBe` Right (Map.fromList
        [("addr_test1qpmw0yhzy093jjcr5qvz87n2jvjw35jprnqejl2wew3ymlprn68ghahcsjmc3flv8jnhka7cjdl7pt9yr0e5ts2z0yfqpe9s5g", 8000)
        ]))

propDelegates404 :: HasServantClientEnv => Expectation
propDelegates404 = serverDelegatesClient "non-existent.delegation" >>= (`shouldBe` Left (DelegationServerError UnknownIp))
