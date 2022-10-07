{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Server.Endpoints.Mint where

import           Control.Monad                    (when)
import           Control.Monad.Catch              (Exception, handle, throwM)
import           Control.Monad.Extra              (forever, unlessM)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Reader             (ask)
import           Common.Logger                    (HasLogger(..), (.<))
import           Common.Tokens                    (Tokens, Token(..))
import           Common.Wait                      (waitTime)
import           Data.Text                        (Text)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.List                        (nub)
import qualified Data.Map                         as M
import           Data.Sequence                    (Seq(..), (|>))
import           GHC.TypeNats                     (Nat)
import           IO.Wallet                        (hasCleanUtxos)
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, WithStatus(..), StdMethod(POST), 
                                                   UVerb, Union, IsMember)
import           Servant.API.Status               (KnownStatus)
import           Server.Internal                  (AppM, Ref, getRef)
import           Server.PostScripts.MintingPolicy (referenceServerPolicy)
import           Server.ServerTx

type MintApi = "relayRequestMint"
            :> ReqBody '[JSON] Tokens
            :> UVerb 'POST '[JSON] MintApiResult

type MintApiResult = '[NoContent, WithStatus 422 Text]

mintHandler :: Tokens -> AppM (Union MintApiResult)
mintHandler tokens = handle mintErrorHandler $ do
    logMsg $ "New mint request received:" .< tokens
    when            hasDuplicates  $ throwM DuplicateTokens
    unlessM (liftIO hasCleanUtxos) $ throwM NoCleanUtxos
    ref <- getRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> tokens))
    respond NoContent
  where
    hasDuplicates = length (nub tokens) /= length tokens

data MintError 
    = DuplicateTokens
    | NoCleanUtxos
    deriving (Show, Exception)

mintErrorHandler :: MintError -> AppM (Union MintApiResult)
mintErrorHandler = \case

    DuplicateTokens -> throwWithStatus @422 $ 
        "The request contains duplicate tokens and will not be processed."
    
    NoCleanUtxos -> throwWithStatus @422 $
        "Balance of pure ada UTxOs in your wallet insufficient to cover \
        \the minimum amount of collateral reuqired."
  where 
    throwWithStatus :: forall (s :: Nat). 
        ( IsMember (WithStatus s Text) MintApiResult
        , KnownStatus s
        ) => Text -> AppM (Union MintApiResult)
    throwWithStatus msg = do
        logMsg msg
        respond (WithStatus @s msg)

newtype QueueM a = QueueM { unQueueM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger QueueM where
    loggerFilePath = "queue.log"

processQueue :: Ref -> IO ()
processQueue ref = unQueueM $ do
    logMsg "Starting queue handler..."
    forever $ liftIO (readIORef ref) >>= \case
        Empty          -> logMsg "No new tokens to process." >> waitTime 3
        tokens :<| tss -> do
            liftIO $ atomicWriteIORef ref tss
            logMsg $ "New tokens to process:" .< tokens
            processTokens tokens

processTokens :: Tokens -> QueueM ()
processTokens ts = mkTxWithConstraints $ do
    TxEnv{..} <- ask
    let bss = map unToken ts
    pure [referenceServerPolicy (fst $ (!!0) $ M.toList txEnvUtxos) bss]