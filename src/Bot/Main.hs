{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Bot.Main where

import           Bot.Opts                        (runWithOpts, Options(..))
import           Control.Monad                   (replicateM)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Common.Logger                   (HasLogger(..), (.<))
import           Common.Wait                     (waitTime)
import           Data.Aeson                      (encode)
import           Data.Functor                    ((<&>))
import           Data.Maybe                      (fromJust)
import           Data.String                     (IsString(fromString))
import qualified Data.Text                       as T
import           ENCOINS.Core.BaseTypes          (MintingPolarity(..), toGroupElement)
import           ENCOINS.Core.Bulletproofs.Types (Inputs, Input(..))
import           Server.Config                   (loadConfig, confServerAddress)
import           System.Random                   (randomRIO, randomIO)
import           Network.HTTP.Client             (httpLbs, defaultManagerSettings, newManager, parseRequest, Manager, Request(..), RequestBody(..))
import           Network.HTTP.Types.Header       (hContentType)

main :: IO ()
main = do
    opts <- runWithOpts
    serverAddress <- confServerAddress <$> loadConfig
    let fullAddress
            = "http://"
            <> T.unpack serverAddress
            <> "/relayRequestMint"
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    unBotM $ do
        logMsg "Starting bot..."
        bot opts nakedRequest manager

newtype BotM a = BotM { unBotM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger BotM where
    loggerFilePath = "bot.log"

bot :: Options -> Request -> Manager -> BotM ()
bot Options{..} nakedReq manager = do
    ts   <- genTokens maxTokensInReq
    let body = RequestBodyLBS $ encode ts
        req = nakedReq
            { method = "POST"
            , requestBody = body
            , requestHeaders = [(hContentType,"application/json")]
            }
    resp <- liftIO $ httpLbs req manager
    logMsg $ "Received response:" .< resp
    waitTime =<< randomRIO (1, averageRequestInterval * 2)
    bot Options{..} req manager

genTokens :: MonadIO m => Int -> m Inputs
genTokens ub = liftIO $ randomRIO (1, ub) >>= flip replicateM genToken

genToken :: IO Input
genToken = liftIO $ do
    len <- randomRIO @Int (1, 8)
    let chars = ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']
    str <- replicateM len $ (chars !!) <$> randomRIO @Int  (0, length chars - 1)
    let inputCommit = fromJust $ toGroupElement $ fromString str
    inputPolarity <- genPolarity
    pure Input{..}

genPolarity :: IO MintingPolarity
genPolarity = randomIO <&> \case
    True  -> Burn
    False -> Mint