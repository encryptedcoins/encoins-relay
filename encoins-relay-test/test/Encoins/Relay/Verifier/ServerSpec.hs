{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Verifier.ServerSpec where

import           Cardano.Server.Client.Handle  (HasServantClientEnv)
import           Control.Monad                 (replicateM)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Maybe                    (fromJust)
import           ENCOINS.BaseTypes             (FieldElementBytes (..), GroupElement (..), MintingPolarity (..), groupIdentity,
                                                toGroupElement)
import           ENCOINS.Bulletproofs          (Proof (..))
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, hashRedeemer)
import           ENCOINS.Core.V1.OffChain      (EncoinsMode (..))
import           Encoins.Relay.Client.Client   (secretsToReqBody, termsToSecrets)
import           Encoins.Relay.Client.Secrets  (randomMintTerm)
import           Encoins.Relay.Verifier.Client (VerifierClientError (..), mkVerifierClientEnv, verifierClient)
import           Encoins.Relay.Verifier.Server (VerifierApiError (..), verifierPrvKey, loadVerifierConfig)
import           Internal                      (runEncoinsServerM)
import           PlutusAppsExtra.Utils.Crypto  (sign)
import           PlutusTx.Extra.ByteString     (toBytes)
import           PlutusTx.Prelude              (sha2_256)
import           System.Random                 (randomRIO)
import           Test.Hspec                    (Expectation, Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = describe "encoins-verifier" $ do

    cEnv <- runIO $ loadVerifierConfig >>= mkVerifierClientEnv

    let ?mode = WalletMode
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
propOk red@(par, input, proof, _) = do
    res <- verifierClient red
    let redOnChain = (par, input, sha2_256 $ toBytes proof, "")
    res `shouldBe` Right (par, input, sha2_256 $ toBytes proof, sign verifierPrvKey $ hashRedeemer redOnChain)

propIncorrectInput :: HasServantClientEnv => EncoinsRedeemer -> Expectation
propIncorrectInput red@(par, _, proof, sig) = do
    res <- verifierClient (par, badInput, proof, sig)
    res `shouldBe` Left (VerifierApiError IncorrectInput)
    where
        badInput = (-42, [("bad1afsafs", Mint), ("bad2iandas", Burn)])

propIncorrectProof :: HasServantClientEnv => EncoinsRedeemer -> Expectation
propIncorrectProof red@(par, input, _, sig) = do
    res <- verifierClient (par, input, badProof, sig)
    res `shouldBe` Left (VerifierApiError IncorrectProof)
    where
        badProof = Proof groupIdentity groupIdentity groupIdentity groupIdentity
            (FieldElementBytes "asdasdsad")
            (FieldElementBytes "adsfgafgf")
            [FieldElementBytes "adsfqqgafgf", FieldElementBytes "adwwwwsfgafgf"]
            []
            (FieldElementBytes "adsfgafgf")