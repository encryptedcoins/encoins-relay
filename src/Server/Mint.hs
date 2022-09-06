{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Server.Mint where

import Data.Sequence
import Data.IORef
import Common.Tokens
import Prelude hiding (head, tail)
import Servant
import Server.Internal
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad

type MintApi = "relayRequestMint" :> ReqBody '[JSON] Tokens :> Post '[JSON] NoContent

mintHandler :: Tokens -> AppM NoContent
mintHandler tokens = NoContent <$ do
    logDebug "New mint request recieved:"
    logDebug $ show tokens
    ref <- getRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> tokens))

processQueue :: Ref -> IO ()
processQueue ref = forever $ readIORef ref >>= \case  
    Empty          -> wait
    tokens :<| tss -> do
        atomicWriteIORef ref tss 
        processTokens tokens

processTokens :: Tokens -> IO ()
processTokens ts = print (sum ts) >> wait

wait :: MonadIO m => m ()
wait = liftIO $ threadDelay waitTime

waitTime :: Int
waitTime = 2_500_000