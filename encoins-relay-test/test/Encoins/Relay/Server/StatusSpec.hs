{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.StatusSpec where

import           Cardano.Server.Client.Client (HasServantClientEnv)
import           Cardano.Server.Config        (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Error         (envelopeToEither)
import           Cardano.Server.Error.Servant (Envelope (..))
import           Cardano.Server.Internal      (ServerM, mkServantClientEnv)
import           Control.Monad                (join)
import           Control.Monad.Catch          (try)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Data                    (Proxy (..))
import           Data.Functor                 ((<&>))
import           Data.WorldPeace              (absurdUnion, openUnion)
import           Encoins.Relay.Client.Opts    (statusClient)
import           Encoins.Relay.Server.Server  (EncoinsApi, embedCreds)
import           Encoins.Relay.Server.Status  (EncoinsStatusError (..), EncoinsStatusReqBody (..), EncoinsStatusResult, StatusApi,
                                               getLedgerEncoins, getMaxAdaWithdraw)
import           Internal                     (runEncoinsServerM)
import           Servant.Client               (ClientError (..), client, responseStatusCode, runClientM)
import           Test.Hspec                   (Expectation, Spec, context, describe, expectationFailure, hspec, it, runIO, shouldBe,
                                               shouldSatisfy)

spec :: Spec
spec = describe "status endpoint" $ do
    Config{..}    <- runIO $ decodeOrErrorFromFile @(Config EncoinsApi) "encoins-relay-test/test/configuration/config.json"
    let ?creds = embedCreds
    cEnv <- runIO $ mkServantClientEnv cPort cHost cHyperTextProtocol
    let ?servantClientEnv = cEnv
    it "max ada withdraw"     $ withStatusSpec MaxAdaWithdraw getMaxAdaWithdraw
    it "ledger encoins utxos" $ withStatusSpec LedgerEncoins  getLedgerEncoins

withStatusSpec :: HasServantClientEnv =>
    EncoinsStatusReqBody -> ServerM EncoinsApi EncoinsStatusResult -> Expectation
withStatusSpec reqBody fun = join $ runEncoinsServerM $ do
    expectation <- try @_ @EncoinsStatusError fun
    runStatusClient reqBody <&> either id (`shouldBe` expectation)

runStatusClient :: HasServantClientEnv =>
    EncoinsStatusReqBody -> ServerM EncoinsApi (Either Expectation (Either EncoinsStatusError EncoinsStatusResult))
runStatusClient reqBody = liftIO (runClientM (client @StatusApi Proxy reqBody) ?servantClientEnv) >>= \case
    Left err -> pure $ Left $ expectationFailure $ show err
    Right (SuccEnvelope res) -> pure $ Right $ Right res
    Right (ErrEnvelope es) -> openUnion
        (openUnion absurdUnion (pure . Right . Left))
        (pure . Left . expectationFailure . show)
        es