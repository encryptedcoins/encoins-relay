{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.ServerSpec where

import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (ServerEndpoint (ServerTxE))
import           Cardano.Server.Internal        (Env (envLogger), ServerM, loadEnv, runServerM)
import           Cardano.Server.Utils.Logger    (logSmth, mutedLogger, (.<))
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Exception              (try)
import           Control.Monad                  (join, replicateM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Bifunctor                 (Bifunctor (bimap, first))
import           Data.Either                    (isLeft, isRight)
import           Data.Fixed                     (Pico)
import           Data.List                      (partition)
import           Data.List.Extra                (dropSuffix, partition)
import           Data.String                    (IsString (..))
import qualified Data.Time                      as Time
import           ENCOINS.BaseTypes              (MintingPolarity (Mint))
import           ENCOINS.Core.V1.OffChain       (EncoinsMode (WalletMode))
import           Encoins.Relay.Client.Client    (TxClientCosntraints, secretsToReqBody, sendTxClientRequest, termsToSecrets,
                                                 txClient)
import           Encoins.Relay.Client.Opts      (EncoinsRequestTerm (RPBurn))
import           Encoins.Relay.Client.Secrets   (getWalletEncoinsTokens, mkSecretFile, randomMintTerm)
import           Encoins.Relay.Server.Server    (EncoinsApi, mkServerHandle)
import           Ledger                         (TokenName)
import           Ledger.Value                   (TokenName (..))
import           PlutusAppsExtra.IO.Wallet      (getWalletAda)
import           System.Directory               (listDirectory)
import           System.Random                  (randomRIO)
import           Test.Hspec                     (Expectation, Spec, context, describe, expectationFailure, hspec, it, shouldBe,
                                                 shouldSatisfy)

spec :: HasServantClientEnv => Spec
spec = describe "encoins server" $ do

    context "wallet mode" $ let ?mode = WalletMode in do

        it "mint tokens" propMint

        it "burn tokens" propBurn

propMint :: TxClientCosntraints ServerTxE => Expectation
propMint = join $ runEncoinsServerM $ do
    startAda <- getWalletAda
    l        <- randomRIO (1,5)
    terms    <- replicateM l randomMintTerm
    secrets  <- termsToSecrets terms
    sendTxClientRequest @ServerTxE secrets >>= \case
        Left err -> pure $ expectationFailure $ show err
        Right _ -> do
            mapM_ (uncurry mkSecretFile) secrets
            (((_,(v, inputs),_,_),_),_) <- secretsToReqBody secrets
            currentTime  <- liftIO Time.getCurrentTime
            tokensMinted <- confirmTokensInWallet currentTime $ map (first TokenName) inputs
            finishAda    <- getWalletAda
            pure $ do
                tokensMinted `shouldBe` True
                compare finishAda startAda `shouldBe` LT

propBurn :: TxClientCosntraints ServerTxE => Expectation
propBurn = join $ runEncoinsServerM $ do
    startAda <- getWalletAda
    terms    <- map (RPBurn . Right . ("secrets/" <>)) <$> liftIO (listDirectory "secrets")
    secrets  <- termsToSecrets terms
    sendTxClientRequest @ServerTxE secrets >>= \case
        Left err -> pure $ expectationFailure $ show err
        Right _ -> do
            (((_,(v, inputs),_,_),_),_) <- secretsToReqBody secrets
            currentTime  <- liftIO Time.getCurrentTime
            tokensBurned <- confirmTokensInWallet currentTime $ map (first TokenName) inputs
            finishAda    <- getWalletAda
            pure $ do
                tokensBurned `shouldBe` True
                compare finishAda startAda `shouldBe` GT

runEncoinsServerM :: ServerM EncoinsApi a -> IO a
runEncoinsServerM ma = do
    env <- mkServerHandle >>= loadEnv
    runServerM env{envLogger = mutedLogger} ma

mintWaitingTime :: Pico -- Seconds
mintWaitingTime = 120

confirmTokensInWallet :: Time.UTCTime -> [(TokenName, MintingPolarity)] -> ServerM EncoinsApi Bool
confirmTokensInWallet startTime tokens = do
    currentTime <- liftIO Time.getCurrentTime
    let (mustBeMinted, mustBeBurnt) = bimap (map fst) (map fst) $ partition ((== Mint) . snd) tokens
    if Time.nominalDiffTimeToSeconds (Time.diffUTCTime currentTime startTime) > mintWaitingTime
    then pure False
    else do
        wTokens <- getWalletEncoinsTokens
        if all (`elem` wTokens) mustBeMinted && all (`notElem` wTokens) mustBeBurnt
        then pure True
        else confirmTokensInWallet startTime tokens