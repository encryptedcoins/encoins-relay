{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Relay.Apps.Ipfs.Client
import           Encoins.Relay.Apps.Ipfs.Config
import           Encoins.Relay.Apps.Ipfs.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro  (AssetMintsAndBurnsData (..),
                                                 AssetMintsAndBurnsResponse (..))

import           Cardano.Api                    (NetworkId (..),
                                                 NetworkMagic (..))
import           Control.Monad.Extra            (forM_, mapMaybeM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (MonadReader (ask),
                                                 ReaderT (..))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC8
import           Data.List.Extra                (unsnoc)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime)
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime,
                                                 posixDayLength,
                                                 utcTimeToPOSIXSeconds)
import           Data.Time.Format
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Servant
import           Servant.Client
import           System.Directory               (createDirectoryIfMissing)
import           System.Directory.Extra         (listFiles)
import           System.FilePath.Posix          (takeFileName, (</>))


type ServerIpfsApi =
         "minted" :> ReqBody '[JSON] Token :> Post '[JSON] Text
    :<|> "burned" :> ReqBody '[JSON] Token :> Post '[JSON] Text

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = minted
           :<|> burned

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
  env <- getIpfsEnv
  run (envPort env) $ app env

-- TODO: get rid of liftIO

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
      assets <- liftIO $ getAssetMintsAndBurns
        (Testnet $ NetworkMagic 1)
        "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
        (fromString $ T.unpack aName)
      liftIO $ print assets
      case getAssetAmount assets of
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

getAssetAmount :: [AssetMintsAndBurnsResponse] -> Maybe Integer
getAssetAmount res =
  let resUniq = if length res == 1
        then snd <$> unsnoc res
        else Nothing
  in ambrAmount . snd <$> (unsnoc . ambrData =<< resUniq)

getAssetTime :: [AssetMintsAndBurnsResponse] -> Maybe UTCTime
getAssetTime res =
  let resUniq = if length res == 1
        then snd <$> unsnoc res
        else Nothing
  in ambrTimestamp . snd <$> (unsnoc . ambrData =<< resUniq)


burned :: Token -> IpfsMonad Text
burned t = do
  liftIO $ putStrLn "Burned token received"
  -- liftIO $ print t
  let mAssetName = name $ pinataMetadata t
  case mAssetName of
    Nothing -> do
      liftIO $ print "No assetName received"
      pure "No assetName received"
    Just aName -> do
      -- liftIO $ print aName
      res <- liftIO $ getAssetMintsAndBurns
        (Testnet $ NetworkMagic 1)
        "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
        (fromString $ T.unpack aName)
      case getAssetTime res of
        Nothing -> do
          liftIO $ putStrLn $ "Unexpected data"
          pure "Unexpected data"

        -- TODO: remove after debug
        Just burnedTime -> do
          env <- ask
          let burnDir = envScheduleDirectory env
          liftIO $ putInQueue burnDir (T.unpack aName) burnedTime
          rottenTokens <- liftIO $ getRottenTokens burnDir
          rottenTokenFiles <- traverse (fetchMetaByStatusAndNameRequest "pinned") rottenTokens
          removeRottenTokens rottenTokenFiles
          pure "Burned token unpinned from ipfs"

        -- Just x
        --   | x < 0 -> do
        --       env <- ask
        --       liftIO $ putInQueue (envScheduleDirectory env <> "/" <> T.unpack aName)
        --       liftIO $ putStrLn "Burned token put into the queue for unpinning"
        --       pure "Burned token unpinned from ipfs"
        --   | otherwise -> do
        --       liftIO $ putStrLn "Token found on the blockchain. Thus it is should not be unpinned"
        --       pure "Not saved"

putInQueue :: FilePath -> FilePath -> UTCTime -> IO ()
putInQueue scheduleDir aName burnedTime = do
  let burnedTimePosix = utcTimeToPOSIXSeconds burnedTime
  let halfDay = posixDayLength / 2
  let timeBytes
        = BSC8.pack
        $ show @Integer
        $ floor
        $ burnedTimePosix + halfDay
  createDirectoryIfMissing False scheduleDir
  let pathQueue = scheduleDir </> aName
  BS.writeFile pathQueue timeBytes


getRottenTokens :: FilePath -> IO [Text]
getRottenTokens path = do
  tokens <- listFiles path
  now <- getPOSIXTime
  mapMaybeM (\t -> selectRottenToken t now <$> BS.readFile t) tokens

selectRottenToken :: FilePath
  -> POSIXTime
  -> ByteString
  -> Maybe Text
selectRottenToken tokenPath now (BSC8.unpack -> burnTime) =
  let mPosixTime =
        parseTimeM @Maybe @POSIXTime True defaultTimeLocale "%s" burnTime
  in case mPosixTime of
    Nothing -> Nothing
    Just pTime
      | pTime <= now -> Just (T.pack $ takeFileName tokenPath)
      | otherwise -> Nothing

removeRottenTokens :: [Either ClientError Files] -> IpfsMonad ()
removeRottenTokens rottenTokenFiles =
  forM_ rottenTokenFiles $ \fToken -> do
    case fToken of
      Left err -> liftIO
        $ putStrLn
        $ "fetchMetaByStatusAndNameRequest error: " <> show err
      Right (rows -> fs) -> do
        liftIO $ print fs
        forM_ fs $ \(ipfsPinHash -> cip) -> do
          eUnpined <- unpinByCipRequest cip
          liftIO $ print eUnpined
          liftIO $ putStrLn "Burned token put into the queue for unpinning"
