{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Server.SumbitTxSpec where

import           Cardano.Server.Client.Client   (HasServantClientEnv)
import           Encoins.Relay.Client.Opts     (submitTxClient)
import           Cardano.Server.Test.Utils      (Malformed (..), shoudlFailWithStatus)
import           Test.Hspec                     (Spec, describe, it)
-- import           Test.Hspec                     (Arbitrary (arbitrary))
import           Test.QuickCheck                (generate)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

spec :: HasServantClientEnv => Spec
spec = describe "/submitTx" $ do

    it "fails with malformed request body" $ do
        Malformed submitTxBody <- generate arbitrary
        submitTxClient submitTxBody `shoudlFailWithStatus` 400