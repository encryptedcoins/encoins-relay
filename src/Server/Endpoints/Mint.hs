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

import           Control.Monad.Catch              (Exception, handle, throwM)
import           Control.Monad.Extra              (forever, unlessM, when)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Reader             (ReaderT(..), MonadReader, asks)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Wait                       (waitTime)
import           Data.Text                        (Text)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.List                        (nub)
import           Data.Sequence                    (Seq(..), (|>))
import           ENCOINS.Core.Bulletproofs.Types  (Input(..))
import           ENCOINS.Core.OffChain            (beaconCurrencySymbol, encoinsTx)
import           ENCOINS.Core.OnChain             (EncoinsRedeemer)
import           IO.Wallet                        (HasWallet(..), hasCleanUtxos)
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, WithStatus(..), StdMethod(POST),
                                                   UVerb, Union)
import           Server.Internal                  (AppM, Env(..), getQueueRef, respondWithStatus)
import           Server.Tx                        (mkTxWithConstraints)
import           Ledger.Typed.Scripts             (Any)

type MintApi = "relayRequestMint"
            :> ReqBody '[JSON] EncoinsRedeemer
            :> UVerb 'POST '[JSON] MintApiResult

type MintApiResult = '[NoContent, WithStatus 422 Text]

mintHandler :: EncoinsRedeemer -> AppM (Union MintApiResult)
mintHandler red@(_, inputs, _) = handle mintErrorHandler $ do
    logMsg $ "New mint request received:\n" .< red
    when     hasDuplicates  $ throwM DuplicateTokens
    unlessM hasCleanUtxos $ throwM NoCleanUtxos
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> red))
    respond NoContent
  where
    hasDuplicates =
        let tokens = map inputCommit inputs
        in  length (nub tokens) /= length tokens

data MintError
    = DuplicateTokens
    | NoCleanUtxos
    deriving (Show, Exception)

mintErrorHandler :: MintError -> AppM (Union MintApiResult)
mintErrorHandler = \case

    DuplicateTokens -> respondWithStatus @422
        "The request contains duplicate tokens and will not be processed."

    NoCleanUtxos -> respondWithStatus @422
        "Balance of pure ada UTxOs in your wallet insufficient to cover \
        \the minimum amount of collateral reuqired."

newtype QueueM a = QueueM { unQueueM :: ReaderT Env IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, HasWallet)

instance HasLogger QueueM where
    loggerFilePath = "queue.log"

runQueueM :: Env -> QueueM () -> IO ()
runQueueM env = flip runReaderT env . unQueueM

processQueue :: Env -> IO ()
processQueue env = runQueueM env $ do
    logMsg "Starting queue handler..."
    qRef <- asks envQueueRef
    forever $ liftIO (readIORef qRef) >>= \case
        Empty          -> logMsg "No new tokens to process." >> waitTime 3
        red :<| rs -> do
            liftIO $ atomicWriteIORef qRef rs
            logMsg $ "New redeemer to process:" .< red
            processTokens red

processTokens :: EncoinsRedeemer -> QueueM ()
processTokens red = do
    params <- asks (beaconCurrencySymbol . envBeaconRef)

    mkTxWithConstraints @Any [encoinsTx params red]