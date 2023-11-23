{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.DelegationSpec where

import           Cardano.Api                            (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import           Cardano.Server.Utils.Logger            (mutedLogger)
import           Cardano.Server.Utils.Wait              (waitTime)
import qualified Control.Concurrent                     as C
import           Control.Exception                      (bracket)
import           Data.Maybe                             (listToMaybe)
import           Encoins.Relay.Apps.Delegation.Internal (Delegation (..), DelegationEnv (DelegationEnv), Progress (..),
                                                         runDelegationM, updateProgress)
import           Encoins.Relay.Apps.Delegation.Server   (runDelegationServer, DelegationServerError (..))
import           System.Directory                       (setCurrentDirectory)
import           Test.Hspec                             (Spec, describe, hspec, it, shouldBe, context, Expectation)
import           Test.QuickCheck                        (Property, Testable (property))
import Cardano.Server.Client.Handle (HasServantClientEnv)
import Encoins.Relay.Apps.Delegation.Client
import qualified Data.Map as Map

spec :: HasServantClientEnv => Spec
spec = describe "Delegation server" $ do

    context "/servers" $ do
        it "gets list of servers" propServers

    context "/current" $ do
        it "gets only servers with amount of delegated tokens more than number, specified in config" propCurrent

    context "/delegates" $ do
        it "gets map with pkhs and balances of the delegates when all is ok" propDelegatesOk
        it "thows 404 to unknown ips" propDelegates404

propServers :: HasServantClientEnv => Expectation
propServers = serversClient >>= (`shouldBe` Right ["test.delegation", "https://0qsdf4aiz2.execute-api.eu-central-1.amazonaws.com/"])

propCurrent :: HasServantClientEnv => Expectation
propCurrent = currentServersClient >>= (`shouldBe` Right ["test.delegation"])

propDelegatesOk :: HasServantClientEnv => Expectation
propDelegatesOk = serverDelegatesClient "test.delegation" >>= (`shouldBe` Right (Map.fromList [("13597b58066918fe10f83c72bf83285c6a6a5cb882ed405bf5e705da", 3)]))

propDelegates404 :: HasServantClientEnv => Expectation
propDelegates404 = serverDelegatesClient "non-existent.delegation" >>= (`shouldBe` Left (DelegationServerError UnknownIp))