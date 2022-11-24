{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Client.Main where

import           Client.Internal           (ClientM, runClientM, Env(..), processPiece, mkRedeemer, genRequestPiece)
import           Client.Opts               (runWithOpts, Options(..), ClientMode(..), AutoOptions(..), ClientRequest, Maximum)
import           Control.Monad             (replicateM)
import           Control.Monad.Reader      (MonadIO(..), forever, when)
import           Data.Aeson                (encode)
import           Data.Functor              ((<&>))
import           Data.List                 (nub)
import           Data.Maybe                (catMaybes)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                            Manager, Request(..), RequestBody(..), responseStatus)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (status204)
import qualified Server.Config             as Server
import           System.Directory          (createDirectoryIfMissing)
import           System.Random             (randomRIO)
import           Utils.Logger              (HasLogger(..), (.<))
import           Utils.Wait                (waitTime)

main :: IO ()
main = do
    Options{..}       <- runWithOpts
    Server.Config{..} <- Server.loadConfig
    createDirectoryIfMissing False "secrets"
    let fullAddress = "http://"
                   <> T.unpack confServerAddress
                   <> "/relayRequestMint"
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    let env = Env confBeaconTxOutRef confWallet
        mkRequest' = mkRequest nakedRequest manager
    runClientM env $ logMsg "Starting client..." >> case mode of
        Manual br            -> mkRequest' br
        Auto AutoOptions{..} -> forever $ do
            br <- genRequest maxTokensInReq
            mkRequest' br
            waitTime =<< randomRIO (1, averageRequestInterval * 2)

mkRequest :: Request -> Manager -> ClientRequest -> ClientM ()
mkRequest nakedReq manager clientReq = do
    logMsg $ "New tokens to send:\n" .< clientReq
    (fileWork, val, inputs)  <- sequence . catMaybes <$> traverse processPiece clientReq
    body <- RequestBodyLBS . encode <$> mkRedeemer inputs val
    let req = nakedReq
            { method = "POST"
            , requestBody = body
            , requestHeaders = [(hContentType, "application/json")]
            }
    resp <- liftIO $ httpLbs req manager
    logMsg $ "Received response:" .< resp
    when (successful resp) $ liftIO fileWork
  where
    successful = (== status204) . responseStatus

genRequest :: MonadIO m => Maximum -> m ClientRequest
genRequest ub = liftIO $ randomRIO (1, ub) >>= flip replicateM genRequestPiece <&> nub