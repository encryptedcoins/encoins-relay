{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Relay.Apps.Ipfs.Types

import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Text                     (Text)
import           Network.Wai.Handler.Warp
import           Servant
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import qualified Network.Wai                          as Wai

type ServerIpfsApi =
         "minted" :> ReqBody '[JSON] Token :> Post '[JSON] Text
    :<|> "burned" :> ReqBody '[JSON] Token :> Post '[JSON] Text

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: Server ServerIpfsApi
serverIpfsApi = minted
           :<|> burned
  where
    minted :: Token -> Handler Text
    minted t = do
      liftIO $ putStrLn "Minted token received"
      liftIO $ print t
      pure "Minted"

    burned :: Token -> Handler Text
    burned t = do
      liftIO $ putStrLn "Burned token received"
      liftIO $ print t
      pure "Burned"

app :: Application
app = corsWithContentType $ serve serverIpfsApiProxy serverIpfsApi

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }

ipfsServer :: IO ()
ipfsServer = run 7000 app
