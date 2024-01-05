{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Relay.Apps.Ipfs.Client
import           Encoins.Relay.Apps.Ipfs.Types

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (ReaderT (..))
import           Data.Text                      (Text)
import           Network.HTTP.Client            hiding (Proxy)
import           Network.HTTP.Client.TLS
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Servant

type ServerIpfsApi =
         "minted" :> ReqBody '[JSON] Token :> Post '[JSON] Text
    :<|> "burned" :> ReqBody '[JSON] Token :> Post '[JSON] Text

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = minted
           :<|> burned

minted :: Token -> IpfsMonad Text
minted t = do
  liftIO $ putStrLn "Minted token received"
  liftIO $ print t
  res <- pinJsonRequest t
  liftIO $ putStrLn "Minted token saved"
  liftIO $ print res
  pure "Minted"

burned :: Token -> IpfsMonad Text
burned t = do
  liftIO $ putStrLn "Burned token received"
  liftIO $ print t
  pure "Burned"


handlerServer :: IpfsEnv -> ServerT ServerIpfsApi Handler
handlerServer env = hoistServer serverIpfsApiProxy (liftIO . flip runReaderT env) serverIpfsApi

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }

app :: IpfsEnv -> Application
app = corsWithContentType . serve serverIpfsApiProxy . handlerServer

ipfsServer :: IO ()
ipfsServer = do
  key <- auth <$> pinataKey "pinata_jwt_token.txt"
  manager <- newManager tlsManagerSettings
  let env = MkIpfsEnv pinUrl fetchUrl key manager
  run 7000 $ app env
