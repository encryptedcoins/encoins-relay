{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Server.Endpoints.Mint where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId (..))
import           Control.Monad                    (when)
import           Control.Monad.Catch              (Exception, handle, throwM)
import           Control.Monad.Extra              (forever, mconcatMapM, unlessM)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.State              (execState)
import           Common.Logger                    (HasLogger(..), (.<))
import           Common.Tokens                    (Tokens, Token(..))
import           Common.Wait                      (waitTime)
import           Data.Aeson                       (decode)
import           Data.Maybe                       (fromJust)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.FileEmbed                   (embedFile)
import           Data.Text                        (Text)
import           Data.Text.Class                  (FromText(fromText))
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.List                        (nub)
import qualified Data.Map                         as M
import           Data.Sequence                    (Seq(..), (|>))
import           GHC.TypeNats                     (Nat)
import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr, hasCleanUtxos)
import           Ledger                           (Params (..))
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, WithStatus(..), StdMethod(POST), UVerb, Union, IsMember)
import           Servant.API.Status               (KnownStatus)
import           Server.Internal                  (AppM, Ref, getRef)
import           Server.PostScripts.MintingPolicy (TestTransaction, TestTransactionBuilder, referenceServerPolicy)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)

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
processTokens ts = do
    bech32 <- liftIO getWalletAddr
    logSmth bech32
    
    let (walletPKH, walletSKH) = case bech32ToKeyHashes <$> fromText bech32 of
            Right (Just res) -> res
            _                -> error "Can't get key hashes from bech32 wallet."
        protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        addrWallet = case bech32ToAddress <$> fromText bech32 of
            Right (Just addr) -> addr
            _                 -> error "Can't get wallet address from bech32 wallet."
    ct <- liftIO currentTime
    utxos <- liftIO $ mconcatMapM getUtxosAt [addrWallet]
    liftIO $ mapM_ (\x -> print x >> putStrLn "\n\n\n\n\n") utxos
    logMsg $ "UTXO amount on addr: " .< length utxos
    let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
        bss = map unToken ts
        txs = [referenceServerPolicy (fst $ (!!0) $ M.toList utxos) bss] 
        constr = fromJust $ execTxs txs constrInit
        (lookups, cons) = fromJust $ txConstructorResult constr

    logMsg "Balancing..."
    balancedTx <- liftIO $ balanceTx ledgerParams lookups cons
    logSmth balancedTx
    logMsg "Signing..."
    signedTx <- liftIO $ signTx balancedTx
    logSmth signedTx
    logMsg "Submitting..."
    liftIO $ submitTxConfirmed signedTx
    logMsg "Submited."

execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs