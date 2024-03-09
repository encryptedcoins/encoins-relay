{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Encoins.Relay.Server.Endpoints.Tx.Server where

import           CSL                                        (TransactionInputs)
import           Cardano.Server.Error                       (ConnectionError, Throws)
import           Cardano.Server.Input                       (InputContext)
import           Cardano.Server.Internal                    (AuxillaryEnvOf, Env, ServerM, runServerM, setLoggerFilePath)
import           Cardano.Server.Tx                          (MkTxConstrains, checkForCleanUtxos, submitTx)
import           Cardano.Server.Utils.Logger                (logMsg, logSmth, (.<))
import           Cardano.Server.Utils.Wait                  (waitTime)
import           Control.Concurrent                         (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception                          (Exception (..), SomeException)
import           Control.Monad                              (join, liftM3, when)
import           Control.Monad.Catch                        (MonadCatch (catch), MonadThrow (throwM), handle)
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Data.IORef                                 (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)
import           Data.Sequence                              (Seq (..), empty, (|>))
import           Data.Time                                  (getCurrentTime)
import qualified Data.Time                                  as Time
import           Encoins.Relay.Server.Endpoints.Tx.Intenral (EncoinsTxApiError, InputOfEncoinsApi, InputWithContext, processRequest,
                                                             txBuilders)
import           Encoins.Relay.Server.Internal              (EncoinsRelayEnv (..), getTrackedAddresses)
import           Servant                                    (JSON, NoContent (..), Post, ReqBody, type (:>))
import Cardano.Server.Handler (wrapHandler)

type ServerTxApi = "serverTx"
    :> Throws EncoinsTxApiError
    :> Throws ConnectionError
    :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
    :> Post '[JSON] NoContent

serverTxHandler :: AuxillaryEnvOf api ~ EncoinsRelayEnv
    => IORef Queue
    -> (InputOfEncoinsApi, TransactionInputs)
    -> ServerM api NoContent
serverTxHandler ref req = wrapHandler @ServerTxApi $ do
        logMsg $ "New serverTx request received:\n" .< req
        qElem <- processRequest req >>= newQueueElem
        liftIO $ atomicModifyIORef ref ((,()) . (|> qElem))
        liftIO (takeMVar $ qeMvar qElem) >>= either reThrow (const $ return NoContent)
    where
        reThrow (fromException @EncoinsTxApiError -> Just txApiErr)  = throwM txApiErr
        reThrow (fromException @ConnectionError    -> Just connError) = throwM connError
        reThrow err                                                   = throwM err

processQueue :: AuxillaryEnvOf api ~ EncoinsRelayEnv => Env api -> IORef Queue -> IO ()
processQueue env qRef = runServerM env $ setLoggerFilePath "queue.log" $ do
        logMsg "Starting queue handler..."
        neverFall go
    where
        neverFall ma = catch ma $ \(err :: SomeException) -> do
            logSmth err
            waitTime 3
            neverFall ma
        go = liftIO getCurrentTime >>= checkQueue
        checkQueue t = liftIO (readIORef qRef) >>= \case
                Empty -> idleQueue t >>= checkQueue
                e :<| es -> processQueueElem qRef e es >> go

idleQueue :: AuxillaryEnvOf api ~ EncoinsRelayEnv => Time.UTCTime -> ServerM api Time.UTCTime
idleQueue st = do
    ct <- liftIO getCurrentTime
    let delta = Time.diffUTCTime ct st
        enoughTimePassed = delta > 300
        firstTime        = delta < 3
    when (enoughTimePassed || firstTime) $ logMsg "No new inputs to process."
    checkForCleanUtxos
    waitTime 3
    pure $ if enoughTimePassed then ct else st

processQueueElem :: (AuxillaryEnvOf api ~ EncoinsRelayEnv, MkTxConstrains (ServerM api))
    => QueueRef -> QueueElem -> Queue -> ServerM api ()
processQueueElem qRef QueueElem{..} elems = handle h $ do
        liftIO $ atomicWriteIORef qRef elems
        logMsg $ "New input to process:" .< qeInput <> "\nContext:" .< qeContext
        checkForCleanUtxos
        join $ liftM3 submitTx getTrackedAddresses (pure qeContext) $ txBuilders qeInput
        liftIO $ putMVar qeMvar $ Right ()
    where
        h = liftIO . putMVar qeMvar . Left

data QueueElem = QueueElem
    { qeInput   :: InputOfEncoinsApi
    , qeContext :: InputContext
    , qeMvar    :: MVar (Either SomeException ())
    }

newQueueElem :: MonadIO m => InputWithContext -> m QueueElem
newQueueElem (qeInput, qeContext) = do
    qeMvar <- liftIO newEmptyMVar
    pure QueueElem{..}

type Queue = Seq QueueElem

type QueueRef = IORef Queue

newQueueRef :: IO QueueRef
newQueueRef = newIORef empty