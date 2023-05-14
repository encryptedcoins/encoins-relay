{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Verifier.Client where

import           Cardano.Server.Client.Client  (createServantClientEnv)
import           Cardano.Server.Client.Handle  (HasServantClientEnv)
import           Cardano.Server.Error          (IsCardanoServerError (..))
import           Cardano.Server.Utils.Logger   ((.<))
import           Control.Exception             (Exception)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Relay.Verifier.Server (VerifierApi, VerifierApiError (..), VerifierConfig (..))
import           Servant                       (Proxy (Proxy), WithStatus (..))
import           Servant.Client                (BaseUrl (..), ClientEnv, ClientError, Scheme (..), baseUrl, client, foldMapUnion,
                                                runClientM)

verifierClient :: HasServantClientEnv => EncoinsRedeemer -> IO (Either VerifierClientError EncoinsRedeemerOnChain)
verifierClient red 
    = (`runClientM` ?servantClientEnv) (foldUnion <$> client (Proxy @VerifierApi) red) >>= \case
        Right (Right red')  -> pure $ Right red'
        Right (Left apiErr) -> pure $ Left $ VerifierApiError apiErr
        Left clientErr      -> pure $ Left $ VerifierClientError clientErr
    where
        foldUnion = foldMapUnion (Proxy @UnUnionVerifierResult) unUnion

mkVerifierClientEnv :: VerifierConfig -> IO ClientEnv
mkVerifierClientEnv VerifierConfig{..} = do
    cEnv <- createServantClientEnv
    pure cEnv{baseUrl = BaseUrl Http (T.unpack cHost) cPort ""}

data VerifierClientError 
    = VerifierApiError VerifierApiError
    | VerifierClientError ClientError
    deriving (Show, Exception, Eq)

instance IsCardanoServerError VerifierClientError where
    errStatus _ = toEnum 422
    errMsg = \case
        VerifierApiError    vErr -> errMsg vErr
        VerifierClientError cErr -> "Encoins verifier is unavailable:" .< cErr

class UnUnionVerifierResult a where
    unUnion :: a -> Either VerifierApiError EncoinsRedeemerOnChain

instance UnUnionVerifierResult (WithStatus 200 EncoinsRedeemerOnChain) where
    unUnion (WithStatus red) = Right red

instance UnUnionVerifierResult (WithStatus 422 Text) where
    unUnion (WithStatus txt)
        | txt == errMsg IncorrectInput = Left IncorrectInput
        | otherwise                    = Left IncorrectProof
