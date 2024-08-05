{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Verifier.Client where

import           Control.Exception             (Exception)
import           Data.Coerce                   (coerce)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Encoins.Common.Constant       (newLine)
import           Encoins.Common.Transform      (toText)
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Relay.Common.Client   (HasServantClientEnv)
import           Encoins.Relay.Common.Error    (IsCardanoServerError (..))
import           Encoins.Relay.Verifier.Server (LegacyAddress (..), LegacyEncoinsRedeemerOnChain, VerifierApiError (..), VerifyEndpoint)
import           Network.HTTP.Client           (defaultManagerSettings, newManager)
import           Servant                       (Proxy (Proxy), WithStatus (..))
import           Servant.Client                (BaseUrl (..), ClientEnv (..), ClientError, Scheme (..), client, defaultMakeClientRequest,
                                                foldMapUnion, runClientM)

verifierClient :: HasServantClientEnv => EncoinsRedeemer -> IO (Either VerifierClientError EncoinsRedeemerOnChain)
verifierClient red
    = (`runClientM` ?servantClientEnv) (foldUnion <$> client (Proxy @VerifyEndpoint) (coerce red)) >>= \case
        Right (Right red')  -> pure $ Right $ coerce red'
        Right (Left apiErr) -> pure $ Left $ VerifierApiError apiErr
        Left clientErr      -> pure $ Left $ VerifierClientError clientErr
    where
        foldUnion = foldMapUnion (Proxy @UnUnionVerifierResult) unUnion

mkVerifierClientEnv :: Text -> Int -> IO ClientEnv
mkVerifierClientEnv host port = do
    m <- newManager defaultManagerSettings
    pure $ ClientEnv
        m
        (BaseUrl Http (T.unpack host) port "")
        Nothing
        defaultMakeClientRequest

data VerifierClientError
    = VerifierApiError VerifierApiError
    | VerifierClientError ClientError
    deriving (Show, Exception, Eq)

instance IsCardanoServerError VerifierClientError where
    errStatus _ = toEnum 422
    errMsg = \case
        VerifierApiError    vErr -> errMsg vErr
        VerifierClientError cErr
            -> "Encoins verifier is unavailable:"
            <> newLine
            <> toText cErr

class UnUnionVerifierResult a where
    unUnion :: a -> Either VerifierApiError LegacyEncoinsRedeemerOnChain

instance UnUnionVerifierResult (WithStatus 200 LegacyEncoinsRedeemerOnChain) where
    unUnion (WithStatus red) = Right red

instance UnUnionVerifierResult (WithStatus 422 Text) where
    unUnion (WithStatus txt)
        | txt == errMsg IncorrectInput = Left IncorrectInput
        | otherwise                    = Left IncorrectProof
