{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Verifier.Client where

import           Cardano.Server.Client.Client  (HasServantClientEnv)
import           Cardano.Server.Error          (IsCardanoServerError (errMsg, errStatus))
import           Cardano.Server.Error.Servant  (Envelope (..))
import           Cardano.Server.Utils.Logger   ((.<))
import           Control.Exception             (Exception)
import           Control.Monad.Identity        (Identity (runIdentity))
import           Data.Functor                  ((<&>))
import           Data.WorldPeace               (absurdUnion, unionRemove)
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Relay.Verifier.Server (VerifierApiError (..), VerifyEndpoint)
import           Servant                       (Proxy (Proxy))
import           Servant.Client                (ClientError, client, runClientM)

verifierClient :: HasServantClientEnv => EncoinsRedeemer -> IO (Either VerifierClientError EncoinsRedeemerOnChain)
verifierClient red
    = (`runClientM` ?servantClientEnv) (client (Proxy @VerifyEndpoint) red) <&> \case
        Right (SuccEnvelope red') -> Right red'
        Right (ErrEnvelope  es)   -> either absurdUnion (Left . VerifierApiError . runIdentity) $ unionRemove @VerifierApiError es
        Left clientError          -> Left $ VerifierClientError clientError

data VerifierClientError
    = VerifierApiError VerifierApiError
    | VerifierClientError ClientError
    deriving (Show, Exception, Eq)

instance IsCardanoServerError VerifierClientError where
    errStatus _ = toEnum 422
    errMsg = \case
        VerifierApiError    vErr -> errMsg vErr
        VerifierClientError cErr -> "Encoins verifier is unavailable:\n" .< cErr