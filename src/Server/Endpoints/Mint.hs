{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
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
import           Common.Logger                    (HasLogger(..), (.<))
import           Common.Tokens                    (Tokens, Token(..))
import           Common.Wait                      (waitTime)
import           Data.Text                        (Text)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.List                        (sort, nub)
import qualified Data.Map                         as M
import           Data.Sequence                    (Seq(..), (|>))
import           GHC.TypeNats                     (Nat)
import           IO.Wallet                        (HasWallet(..), hasCleanUtxos)
import           Ledger                           (TxOutRef)
import           Scripts.Constraints              (referenceMintingPolicyTx)
-- import           Scripts.Constraints              (tokensMintedTx)

import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, WithStatus(..), StdMethod(POST), 
                                                   UVerb, Union, IsMember)
import           Servant.API.Status               (KnownStatus)
import           Server.Config                    (restoreWalletFromConf)
import           Server.Internal                  (AppM, Ref, getRef)
import           Server.ServerTx                  (HasTxEnv, mkTxWithConstraints)

import           Test.OnChain                     (testPolicy)
import           Test.OffChain                    (testToken)
import qualified PlutusTx.Prelude                 as Plutus
import           Ledger.Typed.Scripts             (Any)

type MintApi = "relayRequestMint"
            :> ReqBody '[JSON] Tokens
            :> UVerb 'POST '[JSON] MintApiResult

type MintApiResult = '[NoContent, WithStatus 422 Text]

mintHandler :: Tokens -> AppM (Union MintApiResult)
mintHandler tokens = handle mintErrorHandler $ do
    logMsg $ "New mint request received:" .< tokens
    when     hasDuplicates  $ throwM DuplicateTokens
    unlessM (hasCleanUtxos) $ throwM NoCleanUtxos
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

    DuplicateTokens -> respondWithStatus @422 
        "The request contains duplicate tokens and will not be processed."
    
    NoCleanUtxos -> respondWithStatus @422 
        "Balance of pure ada UTxOs in your wallet insufficient to cover \
        \the minimum amount of collateral reuqired."
  where 
    respondWithStatus :: forall (s :: Nat). 
        ( IsMember (WithStatus s Text) MintApiResult
        , KnownStatus s
        ) => Text -> AppM (Union MintApiResult)
    respondWithStatus msg = do
        logMsg msg
        respond (WithStatus @s msg)

newtype QueueM a = QueueM { unQueueM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger QueueM where
    loggerFilePath = "queue.log"

instance HasWallet QueueM where
    getRestoreWallet = restoreWalletFromConf

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
processTokens ts = mkTxWithConstraints @Any
    [referenceMintingPolicyTx testPolicy utxoRef bss (Plutus.sum $ map testToken bss)]
    -- [tokensMintedTx testPolicy bss (Plutus.sum $ map testToken bss)]
  where
    utxoRef :: HasTxEnv => TxOutRef
    utxoRef = head $ M.keys $ ?txUtxos

    bss = sort $ map unToken ts