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
import           Control.Monad.Extra              (mconcatMapM, forever)
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
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.List                        (nub)
import qualified Data.Map                         as M
import           Data.Sequence                    (Seq(..), (|>))
import           Ledger                           (Params (..))
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, WithStatus(..), StdMethod(POST), UVerb, Union)
import           Server.Internal                  (AppM, Ref, getRef)
import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Server.PostScripts.MintingPolicy (TestTransaction, TestTransactionBuilder, referenceServerPolicy)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)
import           Data.Text.Class                  (FromText(fromText))

type MintApi = "relayRequestMint"
            :> ReqBody '[JSON] Tokens
            :> UVerb 'POST '[JSON] '[NoContent, WithStatus 422 Text]

mintHandler :: Tokens -> AppM (Union '[NoContent, WithStatus 422 Text])
mintHandler tokens = do
    logMsg $ "New mint request received:" .< tokens
    if hasDuplicates

    then do
        let msg = "The request contains duplicate tokens and will not be processed."
        logMsg msg
        respond (WithStatus @422 msg)

    else do
        ref <- getRef
        liftIO $ atomicModifyIORef ref ((,()) . (|> tokens))
        respond NoContent
    where
        hasDuplicates = length (nub tokens) /= length tokens

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