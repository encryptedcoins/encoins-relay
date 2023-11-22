{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.DelegationSpec where

import           Cardano.Api                            (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import           Cardano.Server.Utils.Logger            (mutedLogger)
import           Data.Maybe                             (listToMaybe)
import           Encoins.Relay.Apps.Delegation.Internal (Delegation (..), DelegationEnv (DelegationEnv), Progress (..),
                                                         runDelegationM, updateProgress)
import           System.Directory                       (setCurrentDirectory)
import           Test.Hspec                             (Spec, describe, it, shouldBe)
import           Test.QuickCheck                        (Property, Testable (property))

spec :: Spec
spec = describe "encoins-delegation" $ do
    it "find delegators IP's" propDelegation

propDelegation :: Property
propDelegation = property $ do
    setCurrentDirectory "encoins-relay-test/test/configuration"
    let env = DelegationEnv
            mutedLogger
            Nothing
            (Testnet (NetworkMagic 1))
            "2912c4707b5695db33e60e89467125bda41fecd62c7f8e56cd854247"
            "ENCS"
            False
    p <- runDelegationM env $ updateProgress $ Progress Nothing []
    setCurrentDirectory "../../.."
    listToMaybe (delegIp <$> pDelgations p) `shouldBe` Just "test.delegation"