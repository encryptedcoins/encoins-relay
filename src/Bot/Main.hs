{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Main where

import Bot.Opts
import Control.Exception
import Control.Monad 
import Common.Tokens
import Data.Aeson ( encode )
import Data.FileEmbed
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent
import System.Random
import Network.HTTP.Client
import Network.HTTP.Types.Status

runBot :: IO ()
runBot = do
    opts <- runWithOpts
    let serverAddress 
            = "http://" 
            <> BS8.unpack $(embedFile "testnet/server-address.txt") 
            <> "/relayRequestMint"
    nakedRequest <- parseRequest serverAddress
    manager <- newManager defaultManagerSettings
    bot opts nakedRequest manager

bot :: Options -> Request -> Manager -> IO ()
bot Options{..} req manager = do
    body <- RequestBodyLBS . encode <$> genTokens maxTokensInReq
    resp <- httpLbs req{requestBody = body} manager
    waitTime =<< randomRIO (1, averageRequestInterval * 2)
    bot Options{..} req manager 

genTokens :: Int -> IO [Token]
genTokens ub = randomRIO (1, ub) >>= flip replicateM genToken

genToken :: IO Token
genToken = (tokens!!) <$> randomRIO (0, length tokens - 1)

waitTime :: Int -> IO ()
waitTime = threadDelay . (* 1_000_000)

tokens :: Tokens
tokens = [1..100]