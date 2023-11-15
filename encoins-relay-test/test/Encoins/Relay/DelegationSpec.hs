{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.DelegationSpec where

import           Cardano.Api                        (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import           Data.Maybe                         (listToMaybe)
import           Encoins.Relay.Apps.Delegation.Main (Progress (..), updateProgress)
import           Encoins.Relay.Server.Delegation    (Delegation (..))
import           System.Directory                   (setCurrentDirectory)
import           Test.Hspec                         (Spec, describe, it, shouldBe)
import           Test.QuickCheck                    (Property, Testable (property))

spec :: Spec
spec = describe "encoins-delegation" $ it "find delegators IP's" propDelegation

propDelegation :: Property
propDelegation = property $ do
    setCurrentDirectory "encoins-relay-test/test/configuration"
    let ?networkId = Testnet (NetworkMagic 1)
        ?cs        = "2912c4707b5695db33e60e89467125bda41fecd62c7f8e56cd854247"
        ?tokenName = "ENCS"
        ?checkSig  = False
    p <- updateProgress $ Progress Nothing []
    setCurrentDirectory "../../.."
    listToMaybe (delegIp <$> pDelgations p) `shouldBe` Just "test.delegation"