{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Encoins.Relay.Server.StatusSpec where

import           Cardano.Server.Client.Client   (createServantClientEnv)
import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (statusC)
import           Cardano.Server.Internal        (ServerM)
import           Control.Monad                  (join)
import           Control.Monad.Catch            (try)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Functor                   ((<&>))
import           Encoins.Relay.Server.Status    (EncoinsStatusResult, EncoinsStatusError (..), EncoinsStatusReqBody (..), getMaxAdaWithdraw, getLedgerEncoins, getLedgerTokens)
import           Encoins.Relay.Server.Server    (EncoinsApi)
import           Internal                       (runEncoinsServerM)
import           Servant.Client                 (ClientError (..), responseStatusCode, runClientM)
import           Test.Hspec                     (Expectation, Spec, context, describe, expectationFailure, hspec, it,
                                                 shouldBe, shouldSatisfy, runIO)

spec :: Spec
spec = describe "status endpoint" $ do
    cEnv <- runIO createServantClientEnv
    let ?servantClientEnv = cEnv
    it "max ada withdraw"      $ withStatusSpec MaxAdaWithdrawRequest    getMaxAdaWithdraw
    it "ledger encoins utxos"  $ withStatusSpec LedgerUtxoRequestEncoins getLedgerEncoins
    it "ledger 6 tokens utxos" $ withStatusSpec LedgerUtxoRequestTokens  getLedgerTokens

withStatusSpec :: HasServantClientEnv => 
    EncoinsStatusReqBody -> ServerM EncoinsApi EncoinsStatusResult -> Expectation
withStatusSpec reqBody fun = join $ runEncoinsServerM $ do
    expectation <- try @_ @EncoinsStatusError fun
    runStatusClient reqBody <&> either id (`shouldBe` expectation)

runStatusClient :: HasServantClientEnv => 
    EncoinsStatusReqBody -> ServerM EncoinsApi (Either Expectation (Either EncoinsStatusError EncoinsStatusResult))
runStatusClient reqBody = liftIO (runClientM (statusC @EncoinsApi reqBody) ?servantClientEnv) >>= \case
    Left (resp@(FailureResponse _ (responseStatusCode -> code))) -> 
        if code == toEnum 422 
        then pure $ Right $ Left EmptyLedger
        else pure $ Left $ expectationFailure $ show resp
    Left err -> pure $ Left $ expectationFailure $ show err
    Right res -> pure $ Right $ Right res