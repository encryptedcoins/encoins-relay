{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Server.Endpoints.Mint where

import           Control.Monad.Catch              (Exception, handle, throwM, MonadThrow, MonadCatch)
import           Control.Monad.Extra              (forever, unlessM)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Reader             (ReaderT(..), MonadReader, asks)
import           Data.Kind                        (Type)
import           Data.Text                        (Text)
import           Data.Typeable                    (Typeable)
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

mintHandler :: forall s. HasMintEndpoint s => RedeemerOf s -> AppM s (Union MintApiResult)
mintHandler red = handle mintErrorHandler $ do
    logMsg $ "New mint request received:\n" .< red
    checkForMintErros red
    unlessM hasCleanUtxos $ throwM $ NoCleanUtxos @s
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> red))
    respond NoContent

class (HasServer s
      , Typeable s
      , Show (MintErrorOf s)
      , Exception (MintErrorOf s)
      ) => HasMintEndpoint s where

    data MintErrorOf s :: Type

    checkForMintErros :: RedeemerOf s -> AppM s ()

    handleSpecificError :: MintErrorOf s -> AppM s (Union MintApiResult)

data MintError s
    = NoCleanUtxos
    | SpecificMintError (MintErrorOf s)
deriving instance Show (MintErrorOf s) => Show (MintError s)
deriving instance (Exception (MintErrorOf s), Typeable s) => Exception (MintError s)

mintErrorHandler :: HasMintEndpoint s => MintError s -> AppM s (Union MintApiResult)
mintErrorHandler = \case
    
    NoCleanUtxos -> respondWithStatus @422
        "Balance of pure ada UTxOs in your wallet insufficient to cover \
        \the minimum amount of collateral reuqired."

    SpecificMintError se -> handleSpecificError se

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