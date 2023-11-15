{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<"                #-}
{-# HLINT ignore "Redundant <$>"          #-}
{-# HLINT ignore "Use fewer imports" #-}

module Encoins.Relay.Server.ServerSpec where

import           Cardano.Server.Client.Handle          (HasServantClientEnv)
import           Cardano.Server.Config                 (ServerEndpoint (ServerTxE), decodeOrErrorFromFile)
import           Cardano.Server.Internal               (Env (envLogger), ServerM, loadEnv, runServerM)
import           Cardano.Server.Utils.Logger           (logSmth, mutedLogger, (.<))
import           Cardano.Server.Utils.Wait             (waitTime)
import           Control.Exception                     (try)
import           Control.Monad                         (join, replicateM)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Bifunctor                        (Bifunctor (bimap, first))
import           Data.Either                           (isLeft, isRight)
import           Data.Fixed                            (Pico)
import           Data.List                             (partition)
import           Data.List.Extra                       (dropSuffix, partition)
import           Data.String                           (IsString (..))
import qualified Data.Time                             as Time
import           ENCOINS.BaseTypes                     (MintingPolarity (Mint))
import           ENCOINS.Core.OffChain                 (EncoinsMode (..))
import           Encoins.Relay.Apps.Delegation.V1.Main (DelegationHandle (..), findDelegators, getTokenBalanceIO,
                                                        mkDelegationHandle)
import           Encoins.Relay.Apps.Internal           (encoinsCS, encoinsTokenName)
import           Encoins.Relay.Client.Client           (TxClientCosntraints, secretsToReqBody, sendTxClientRequest,
                                                        termsToSecrets, txClientRedeemer)
import           Encoins.Relay.Client.Opts             (EncoinsRequestTerm (RPBurn))
import           Encoins.Relay.Client.Secrets          (HasEncoinsModeAndBulletproofSetup, getEncoinsTokensFromMode, mkSecretFile,
                                                        randomMintTermWithUB)
import           Encoins.Relay.Server.Config           (EncoinsRelayConfig (..), loadEncoinsRelayConfig)
import           Encoins.Relay.Server.Delegation       (Delegation (delegIp))
import           Encoins.Relay.Server.Server           (EncoinsApi, mkServerHandle)
import           Internal                              (runEncoinsServerM)
import           Plutus.V2.Ledger.Api                  (Credential (PubKeyCredential), StakingCredential (..), TokenName (..))
import           PlutusAppsExtra.IO.ChainIndex         (getAdaAt, getValueAt)
import           PlutusAppsExtra.IO.Wallet             (getWalletAda, getWalletAddr, getWalletValue)
import           PlutusAppsExtra.Utils.Address         (getStakeKey)
import           PlutusAppsExtra.Utils.Time            (utcToSlot)
import qualified PlutusTx.AssocMap                     as PAM
import           System.Directory                      (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import           System.Random                         (randomRIO)
import           Test.Hspec                            (Expectation, HasCallStack, Spec, context, describe, expectationFailure,
                                                        hspec, it, pendingWith, runIO, shouldBe, shouldSatisfy)
import           Test.Hspec.Core.Spec                  (sequential)
import           Test.QuickCheck                       (Arbitrary (..), choose, generate)

spec :: (HasCallStack, HasServantClientEnv) => Spec
spec = do
    describe "serverTx endpoint" $ do

        context "redeemer" $ do

            bulletproofSetup <- runIO $ decodeOrErrorFromFile "encoins-relay-test/test/configuration/bulletproof_setup.json"
            let ?bulletproofSetup = bulletproofSetup

            context "wallet mode" $ let ?mode = WalletMode in sequential $ do

                it "mint tokens" propMint

                it "burn tokens" propBurn

            context "ledger mode" $ let ?mode = LedgerMode in sequential $ do

                -- it "mint tokens" propMint
                it "mint tokens" $ pendingWith "Pending until encoins-core ledger mode fix."

                -- it "burn tokens" propBurn
                it "burn tokens" $ pendingWith "Pending until encoins-core ledger mode fix."

propMint :: (TxClientCosntraints ServerTxE, HasEncoinsModeAndBulletproofSetup) => Expectation
propMint = join $ runEncoinsServerM $ do
    l        <- randomRIO (1, 2)
    terms    <- replicateM l $ randomMintTermWithUB 5
    secrets  <- termsToSecrets terms
    liftIO $ createDirectoryIfMissing True "secrets"
    sendTxClientRequest @ServerTxE secrets >>= \case
        Left err -> pure $ expectationFailure $ show err
        Right _ -> do
            mapM_ (uncurry mkSecretFile) secrets
            ((_,(v, inputs),_,_),_) <- secretsToReqBody secrets
            currentTime  <- liftIO Time.getCurrentTime
            tokensMinted <- confirmTokens currentTime $ map (first TokenName) inputs
            pure $ tokensMinted `shouldBe` True

propBurn :: (TxClientCosntraints ServerTxE, HasEncoinsModeAndBulletproofSetup) => Expectation
propBurn = join $ runEncoinsServerM $ do
    terms    <- map (RPBurn . Right . ("secrets/" <>)) <$> liftIO (listDirectory "secrets")
    secrets  <- termsToSecrets terms
    liftIO $ removeDirectoryRecursive "secrets"
    sendTxClientRequest @ServerTxE secrets >>= \case
        Left err -> pure $ expectationFailure $ show err
        Right _ -> do
            ((_,(v, inputs),_,_),_) <- secretsToReqBody secrets
            currentTime  <- liftIO Time.getCurrentTime
            tokensBurned <- confirmTokens currentTime $ map (first TokenName) inputs
            pure $ tokensBurned `shouldBe` True

maxConfirmationTime :: Pico -- Seconds
maxConfirmationTime = 300

confirmTokens :: HasEncoinsModeAndBulletproofSetup => Time.UTCTime -> [(TokenName, MintingPolarity)] -> ServerM EncoinsApi Bool
confirmTokens startTime tokens = do
    currentTime <- liftIO Time.getCurrentTime
    let (mustBeMinted, mustBeBurnt) = bimap (map fst) (map fst) $ partition ((== Mint) . snd) tokens
    if Time.nominalDiffTimeToSeconds (Time.diffUTCTime currentTime startTime) > maxConfirmationTime
    then pure False
    else do
        tokensIn <- getEncoinsTokensFromMode
        if all (`elem` tokensIn) mustBeMinted && all (`notElem` tokensIn) mustBeBurnt
        then pure True
        else confirmTokens startTime tokens