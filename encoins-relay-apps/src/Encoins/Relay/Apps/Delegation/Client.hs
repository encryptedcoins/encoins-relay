{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Encoins.Relay.Apps.Delegation.Client where

import           Cardano.Server.Client.Handle           (HasServantClientEnv)
import           Cardano.Server.Config                  (decodeOrErrorFromFile, schemeFromProtocol, HyperTextProtocol)
import           Cardano.Server.Internal                (mkServantClientEnv)
import           Control.Exception                      (Exception)
import           Data.Bifunctor                         (Bifunctor (..))
import qualified Data.ByteString.Lazy                   as BS
import           Data.Either.Extra                      (eitherToMaybe)
import           Data.Functor                           ((<&>))
import           Data.Map                               (Map)
import           Data.Proxy                             (Proxy (..))
import           Data.String                            (IsString (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Text.Encoding                     (decodeUtf8')
import           Encoins.Relay.Apps.Delegation.Internal (DelegConfig (..))
import           Encoins.Relay.Apps.Delegation.Server   (DelegationServerError, GetCurrentServers, GetServerDelegators,
                                                         GetServers, readDelegationServerError)
import           Network.HTTP.Client                    (defaultManagerSettings, newManager)
import           Servant.Client                         (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientError (FailureResponse),
                                                         ClientM, ResponseF (Response), client,
                                                         defaultMakeClientRequest, runClientM)
import           System.Environment                     (getArgs)

main :: FilePath -> IO ()
main delegConfigFp = do
    DelegConfig{..} <- decodeOrErrorFromFile delegConfigFp
    clientEnv <- mkServantClientEnv cPort cHost cHyperTextProtocol
    let ?servantClientEnv = clientEnv
    getArgs >>= \case
        ["servers"] -> serversClient >>= print
        ["current"] -> currentServersClient >>= print
        [ip]        -> serverDelegatesClient (fromString ip) >>= print
        args        -> error $ "unknown args:\n" <> show args

serversClient :: HasServantClientEnv => IO (Either DelegationClientError (Map Text Integer))
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