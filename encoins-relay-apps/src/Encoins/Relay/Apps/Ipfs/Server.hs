{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Relay.Apps.Ipfs.Client
import           Encoins.Relay.Apps.Ipfs.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro  (AssetMintsAndBurnsData (..),
                                                 AssetMintsAndBurnsResponse (..))

import           Cardano.Api                    (NetworkId (..),
                                                 NetworkMagic (..))
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (MonadReader (ask),
                                                 ReaderT (..))
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC8
import           Data.List.Extra                (unsnoc)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text, unpack)
import qualified Data.Text.Encoding             as TE
import           Data.Time.Clock.POSIX          (getPOSIXTime, posixDayLength)
import           Network.HTTP.Client            hiding (Proxy)
import           Network.HTTP.Client.TLS
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Plutus.V2.Ledger.Api           (CurrencySymbol, TokenName)
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
  let mAssetName = name $ pinataMetadata t
  case mAssetName of
    Nothing -> do
      liftIO $ print "No assetName received"
      pure "No assetName received"
    Just aName -> do
      liftIO $ print aName
      mintBurnResp <- liftIO $ assetAmount
        (Testnet $ NetworkMagic 1)
        "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
        (fromString $ unpack aName)
      liftIO $ print mintBurnResp
      case mintBurnResp of
        Nothing -> do
          liftIO $ putStrLn $ "Unexpected data"
          pure "Unexpected data"
        Just x
          | x > 0 -> do
              res <- pinJsonRequest t
              liftIO $ putStrLn "Minted token saved"
              liftIO $ print res
              pure "Minted token saved to ipfs"
          | otherwise -> do
              liftIO $ putStrLn "Token not found in blockchain. Thus it is not saved to ipfs"
              pure "Not saved"


assetAmount :: NetworkId -> CurrencySymbol -> TokenName -> IO (Maybe Integer)
assetAmount netId policyId tokenName = do
  res <- getAssetMintsAndBurns netId policyId tokenName
  let resUniq = if length res == 1
        then snd <$> unsnoc res
        else Nothing
  pure $ ambrAmount . snd <$> (unsnoc . ambrData =<< resUniq)


burned :: Token -> IpfsMonad Text
burned t = do
  liftIO $ putStrLn "Burned token received"
  liftIO $ print t
  let mAssetName = name $ pinataMetadata t
  case mAssetName of
    Nothing -> do
      liftIO $ print "No assetName received"
      pure "No assetName received"
    Just aName -> do
      liftIO $ print aName
      res <- liftIO $ assetAmount
        (Testnet $ NetworkMagic 1)
        "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
        (fromString $ unpack aName)
      case res of
        Nothing -> do
          liftIO $ putStrLn $ "Unexpected data"
          pure "Unexpected data"
        Just x
          | x < 0 -> do
              env <- ask
              liftIO $ putInQueue (envScheduleDir env <> "/" <> unpack aName)
              liftIO $ putStrLn "Burned token put into the queue for unpinning"
              pure "Burned token unpinned from ipfs"
          | otherwise -> do
              liftIO $ putStrLn "Token found on the blockchain. Thus it is should not be unpinned"
              pure "Not saved"

putInQueue :: FilePath -> IO ()
putInQueue schedulePath = do
  time <- getPOSIXTime
  let halfDay = posixDayLength / 2
  let timeBytes = BSC8.pack $ show $ time + halfDay
  BS.writeFile schedulePath timeBytes


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
  let env = MkIpfsEnv pinUrl fetchUrl key manager "schedule"
  run 7000 $ app env
