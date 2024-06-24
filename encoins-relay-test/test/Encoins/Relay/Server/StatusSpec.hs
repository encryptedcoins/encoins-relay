{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Encoins.Relay.Server.StatusSpec where

import           Encoins.Relay.Common.Client          (HasServantClientEnv)
import           Control.Monad                        (join)
import           Control.Monad.Catch                  (try)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Data                            (Proxy (..))
import           Data.Functor                         ((<&>))
import           Encoins.Relay.Common                 (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Error.Envelope  (Envelope (..))
import           Encoins.Relay.Common.Server          (mkServantClientEnv)
import           Encoins.Relay.Server.Config          (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Endpoint.Status (EncoinsStatusError (..), EncoinsStatusReqBody (..), EncoinsStatusResult,
                                                       StatusEndpoint, getLedgerEncoins, getMaxAdaWithdraw)
import           Encoins.Relay.Server.Internal        (EncoinsServerM)
import           Encoins.Relay.Server.Server          (EncoinsApi)
import           Internal                             (runEncoinsServerM)
import           Servant.Client                       (ClientError (..), client, responseStatusCode, runClientM)
import           Test.Hspec                           (Expectation, Spec, context, describe, expectationFailure, hspec, it, runIO, shouldBe,
                                                       shouldSatisfy)

spec :: HasServantClientEnv => Spec
spec = describe "status endpoint" $ do
    it "max ada withdraw" maxAdaWithdrawSpec
    it "ledger encoins utxos" ledgerEncoinsSpec

maxAdaWithdrawSpec :: HasServantClientEnv => Expectation
maxAdaWithdrawSpec = runEncoinsServerM $ do
    res <- liftIO (runClientM (client (Proxy @StatusEndpoint) MaxAdaWithdraw) ?servantClientEnv)
    expected <- getMaxAdaWithdraw
    liftIO $ case res of
        Right (SuccEnvelope r) -> r `shouldBe` expected
        err                    -> expectationFailure $ show err

ledgerEncoinsSpec :: HasServantClientEnv => Expectation
ledgerEncoinsSpec = runEncoinsServerM $ do
    res <- liftIO (runClientM (client (Proxy @StatusEndpoint) LedgerEncoins) ?servantClientEnv)
    expected <- getLedgerEncoins
    liftIO $ case res of
        Right (SuccEnvelope r) -> r `shouldBe` expected
        err                    -> expectationFailure $ show err