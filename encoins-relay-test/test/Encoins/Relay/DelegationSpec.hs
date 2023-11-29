{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.DelegationSpec where

import           Cardano.Api                            (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import           Cardano.Server.Client.Handle           (HasServantClientEnv)
import           Cardano.Server.Utils.Logger            (mutedLogger)
import           Cardano.Server.Utils.Wait              (waitTime)
import qualified Control.Concurrent                     as C
import           Control.Exception                      (bracket)
import qualified Data.Map                               as Map
import           Data.Maybe                             (listToMaybe)
import           Encoins.Relay.Apps.Delegation.Client   (DelegationClientError (DelegationServerError), currentServersClient,
                                                         serverDelegatesClient, serversClient)
import           Encoins.Relay.Apps.Delegation.Internal (Delegation (..), DelegationEnv (DelegationEnv), Progress (..),
                                                         runDelegationM, updateProgress)
import           Encoins.Relay.Apps.Delegation.Server   (DelegationServerError (..), runDelegationServer)
import           System.Directory                       (setCurrentDirectory)
import           Test.Hspec                             (Expectation, Spec, context, describe, hspec, it, shouldBe)
import           Test.QuickCheck                        (Property, Testable (property))

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
    [ ("test.delegation", 3)
    , ("https://0qsdf4aiz2.execute-api.eu-central-1.amazonaws.com/", 2)
    ]))

propCurrent :: HasServantClientEnv => Expectation
propCurrent = currentServersClient >>= (`shouldBe` Right ["test.delegation"])

propDelegatesOk :: HasServantClientEnv => Expectation
propDelegatesOk = serverDelegatesClient "test.delegation" >>=
    (`shouldBe` Right (Map.fromList [("addr_test1qrzde3sqw0na9n4p8dmhc55gkphk2x48hzclpjx6cy0wuzgnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdqghxhek", 3)]))

propDelegates404 :: HasServantClientEnv => Expectation
propDelegates404 = serverDelegatesClient "non-existent.delegation" >>= (`shouldBe` Left (DelegationServerError UnknownIp))