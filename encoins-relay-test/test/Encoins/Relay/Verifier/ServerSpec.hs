{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Verifier.ServerSpec where

import           Cardano.Server.Client.Handle  (HasServantClientEnv)
import           Cardano.Server.Config         (decodeOrErrorFromFile)
import           Control.Monad                 (replicateM)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Either                   (isLeft)
import           Data.Maybe                    (fromJust)
import           ENCOINS.BaseTypes             (FieldElementBytes (..), GroupElement (..), MintingPolarity (..), groupIdentity,
                                                toGroupElement)
import           ENCOINS.Bulletproofs          (Proof (..))
import           ENCOINS.Core.OffChain         (EncoinsMode (..), mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, hashRedeemer)
import           Encoins.Relay.Client.Client   (secretsToReqBody, termsToSecrets)
import           Encoins.Relay.Client.Secrets  (randomMintTerm)
import           Encoins.Relay.Verifier.Client (VerifierClientError (..), mkVerifierClientEnv, verifierClient)
import           Encoins.Relay.Verifier.Server (VerifierApiError (..), VerifierConfig (..))
import           Internal                      (runEncoinsServerM)
import           PlutusAppsExtra.Utils.Crypto  (sign)
import           PlutusTx.Extra.ByteString     (toBytes)
import           PlutusTx.Prelude              (sha2_256)
import           System.Random                 (randomRIO)
import           Test.Hspec                    (Expectation, Spec, describe, it, runIO, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "encoins-verifier" $ do
    VerifierConfig{..} <- runIO $ decodeOrErrorFromFile "encoins-relay-test/test/configuration/verifierConfig.json"
    cEnv             <- runIO $ mkVerifierClientEnv cHost cPort
    bulletproofSetup <- runIO $ decodeOrErrorFromFile "encoins-relay-test/test/configuration/bulletproof_setup.json"

    let ?mode = WalletMode
        ?bulletproofSetup = bulletproofSetup
        ?servantClientEnv = cEnv

    (red, _) <- runIO $ runEncoinsServerM $ do
        l        <- randomRIO (1,4)
        terms    <- replicateM l randomMintTerm
        secrets  <- termsToSecrets terms
        secretsToReqBody secrets

    it "adds signature when all is ok" $ propOk red

    it "handles incorrect input" $ propIncorrectInput red

    it "handles incorrect proof" $ propIncorrectProof red

propOk :: HasServantClientEnv => EncoinsRedeemer -> Expectation
propOk red = do
    res <- verifierClient red
    verifierPrvKey <- decodeOrErrorFromFile "encoins-relay-test/test/configuration/verifierPrvKey.json"
    res `shouldBe` Right (mkEncoinsRedeemerOnChain verifierPrvKey red)

propIncorrectInput :: HasServantClientEnv => EncoinsRedeemer -> Expectation
propIncorrectInput (par, input, proof, sig) = do
    res <- verifierClient (par, badInput, proof, sig)
    res `shouldSatisfy` isLeft
    where
        badInput = (fst input - 1, snd input)

propIncorrectProof :: HasServantClientEnv => EncoinsRedeemer -> Expectation
propIncorrectProof (par, input, _, sig) = do
    res <- verifierClient (par, input, badProof, sig)
    res `shouldSatisfy` isLeft
    where
        badProof = Proof groupIdentity groupIdentity groupIdentity groupIdentity
            (FieldElementBytes "asdasdsad")
            (FieldElementBytes "adsfgafgf")
            [FieldElementBytes "adsfqqgafgf", FieldElementBytes "adwwwwsfgafgf"]
            []
            (FieldElementBytes "adsfgafgf")