{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Encoins.Relay.Apps.Delegation.Client where

import           Cardano.Server.Client.Handle         (HasServantClientEnv)
import           Control.Exception                    (Exception)
import           Data.Bifunctor                       (Bifunctor (..))
import qualified Data.ByteString.Lazy                 as BS
import           Data.Either.Extra                    (eitherToMaybe)
import           Data.Functor                         ((<&>))
import           Data.Map                             (Map)
import           Data.Proxy                           (Proxy (..))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8')
import           Encoins.Relay.Apps.Delegation.Server (DelegationServerError, GetCurrentServers, GetServerDelegators, GetServers,
                                                       readDelegationServerError)
import           Network.HTTP.Client                  (defaultManagerSettings, newManager)
import           Servant.Client                       (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientError (FailureResponse),
                                                       ClientM, ResponseF (Response), Scheme (Http), client,
                                                       defaultMakeClientRequest, runClientM)

serversClient :: HasServantClientEnv => IO (Either DelegationClientError [Text])
serversClient = runDelegationClient $ client (Proxy @GetServers)

currentServersClient :: HasServantClientEnv => IO (Either DelegationClientError [Text])
currentServersClient = runDelegationClient $ client (Proxy @GetCurrentServers)

serverDelegatesClient :: HasServantClientEnv => Text -> IO (Either DelegationClientError (Map Text Integer))
serverDelegatesClient ip = runDelegationClient $ client (Proxy @GetServerDelegators) ip

runDelegationClient :: HasServantClientEnv => ClientM a -> IO (Either DelegationClientError a)
runDelegationClient c = (c `runClientM` ?servantClientEnv) <&> first fromClientError
    where
        fromClientError = \case
            (FailureResponse _ (Response _ _ _ (readServerError -> Just err))) -> DelegationServerError err
            err -> DelegationClientError err
        readServerError bs = eitherToMaybe (decodeUtf8' $ BS.toStrict bs) >>= readDelegationServerError

data DelegationClientError
    = DelegationServerError DelegationServerError
    | DelegationClientError ClientError
    deriving (Show, Exception, Eq)

mkDelegationClientEnv :: Text -> Int -> IO ClientEnv
mkDelegationClientEnv host port = do
    m <- newManager defaultManagerSettings
    pure $ ClientEnv
        m
        (BaseUrl Http (T.unpack host) port "")
        Nothing
        defaultMakeClientRequest