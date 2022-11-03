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
import           Common.Logger                    (HasLogger(..), (.<))
import           Common.Wait                      (waitTime)
import           Data.Text                        (Text)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.List                        (nub)
import           Data.Maybe                       (fromJust)
import           Data.Sequence                    (Seq(..), (|>))
import           Data.String                      (fromString)
import           ENCOINS.Core.BaseTypes           (toFieldElement, toGroupElement)
import           ENCOINS.Core.Bulletproofs.Types  (Inputs, Input(..), Proof(..))
import           ENCOINS.Core.OffChain            (beaconCurrencySymbol, encoinsTx)
import           IO.Wallet                        (HasWallet(..), hasCleanUtxos)
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, WithStatus(..), StdMethod(POST), 
                                                   UVerb, Union)
import           Server.Internal                  (AppM, Env(..), getQueueRef, respondWithStatus)
import           Server.ServerTx                  (mkTxWithConstraints)
import           Ledger.Typed.Scripts             (Any)

type MintApi = "relayRequestMint"
            :> ReqBody '[JSON] Inputs
            :> UVerb 'POST '[JSON] MintApiResult

type MintApiResult = '[NoContent, WithStatus 422 Text]

mintHandler :: Inputs -> AppM (Union MintApiResult)
mintHandler inputs = handle mintErrorHandler $ do
    logMsg $ "New mint request received:" .< inputs
    when     hasDuplicates  $ throwM DuplicateTokens
    unlessM (hasCleanUtxos) $ throwM NoCleanUtxos
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> inputs))
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
        inputs :<| iss -> do
            liftIO $ atomicWriteIORef qRef iss
            logMsg $ "New tokens to process:" .< inputs
            processTokens inputs

processTokens :: Inputs -> QueueM ()
processTokens inputs = do
    envBeaconRef <- asks envBeaconRef
    mkTxWithConstraints @Any $ 
        let encoinsParams = beaconCurrencySymbol envBeaconRef
            txParams  = ( 2_000_000
                        , ?txWalletAddr
                        , ?txWalletPKH
                        , (0, ?txCt + 1_849_863_000)
                        )
            dummyFE   = toFieldElement 100
            dummyGE   = fromJust $ toGroupElement $ fromString "aaaa"
            dummyProof = Proof dummyGE dummyGE dummyGE dummyGE dummyFE dummyFE dummyFE [dummyFE] [dummyFE]
            encoinsRedeemer = (txParams, inputs, dummyProof)
        in [encoinsTx encoinsParams encoinsRedeemer]