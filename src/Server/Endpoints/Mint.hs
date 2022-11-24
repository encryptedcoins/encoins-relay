{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Server.Endpoints.Mint where

import           Control.Monad.Catch              (Exception, handle, throwM, MonadThrow, MonadCatch)
import           Control.Monad.Extra              (forever, unlessM)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Reader             (ReaderT(..), MonadReader, asks)
import           Data.Text                        (Text)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.Sequence                    (Seq(..), (|>))
import           IO.Wallet                        (HasWallet(..), hasCleanUtxos)
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond,
                                                   WithStatus(..), StdMethod(POST), UVerb, Union)
import           Server.Internal                  (getQueueRef, AppM, Env(envQueueRef), HasServer(..), QueueRef)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Servant                    (respondWithStatus)
import           Utils.Wait                       (waitTime)

type MintApi s = "relayRequestMint"
              :> ReqBody '[JSON] (RedeemerOf s)
              :> UVerb 'POST '[JSON] MintApiResult

type MintApiResult = '[NoContent, WithStatus 422 Text]

mintHandler :: HasServer s => RedeemerOf s -> AppM s (Union MintApiResult)
mintHandler red = handle mintErrorHandler $ do
    logMsg $ "New mint request received:\n" .< red
    -- when    hasDuplicates $ throwM DuplicateTokens
    unlessM hasCleanUtxos $ throwM NoCleanUtxos
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> red))
    respond NoContent
--   where
    -- hasDuplicates =
    --     let tokens = map inputCommit inputs
    --     in  length (nub tokens) /= length tokens

data MintError
    = DuplicateTokens
    | NoCleanUtxos
    deriving (Show, Exception)

mintErrorHandler :: MintError -> AppM s (Union MintApiResult)
mintErrorHandler = \case

    DuplicateTokens -> respondWithStatus @422
        "The request contains duplicate tokens and will not be processed."
    
    NoCleanUtxos -> respondWithStatus @422
        "Balance of pure ada UTxOs in your wallet insufficient to cover \
        \the minimum amount of collateral reuqired."

newtype QueueM s a = QueueM { unQueueM :: ReaderT (Env s) IO a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , MonadThrow
        , MonadCatch
        , HasWallet
        )

instance HasLogger (QueueM s) where
    loggerFilePath = "queue.log"

runQueueM :: Env s -> QueueM s () -> IO ()
runQueueM env = flip runReaderT env . unQueueM

processQueue :: forall s. HasServer s => Env s -> IO ()
processQueue env = runQueueM env $ do
    logMsg "Starting queue handler..."
    handle (\(_ :: IOError) -> go) go
  where
    go = do
        qRef <- asks envQueueRef
        forever $ liftIO (readIORef qRef) >>= \case
            Empty        -> logMsg "No new redeemers to process." >> waitTime 3
            red :<| reds -> processRedeemer qRef red reds

processRedeemer :: HasServer s => QueueRef s -> RedeemerOf s -> Seq (RedeemerOf s) -> QueueM s ()
processRedeemer qRef red reds = do
    liftIO $ atomicWriteIORef qRef reds
    logMsg $ "New redeemer to process:" .< red
    processTokens red