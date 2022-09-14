{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Server.Mint where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId (..))
import           Control.Monad.Extra              (mconcatMapM, forever, Monad)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.State              (execState)
import           Common.Logger                    (HasLogger(..), (.<))
import           Common.Tokens                    (Tokens, unToken)
import           Common.Wait                      (waitTime)
import           Data.Aeson                       (decode)
import           Data.Maybe                       (fromJust)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.FileEmbed                   (embedFile)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.Sequence                    (Seq(..), (|>))
import           Ledger                           (Params (..))
import           PlutusTx.Prelude                 hiding (Semigroup(..), Eq (..), (<$>), (<$), unless, mapMaybe, find, toList, fromInteger, mempty,
                                                   concatMap, Functor, Applicative(..), error)
import           Prelude                          (IO,(<$>), (<$), Functor, Applicative(..), error)
import           Servant                          (NoContent(..), JSON, Post, (:>), ReqBody)
import           Server.Internal                  (AppM, Ref, getRef)
import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Test.OffChain                    (TestTransaction, TestTransactionBuilder, testMintTx)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)
import           Data.Text.Class                  (FromText(fromText))

type MintApi = "relayRequestMint" :> ReqBody '[JSON] Tokens :> Post '[JSON] NoContent

mintHandler :: Tokens -> AppM NoContent
mintHandler tokens = NoContent <$ do
    logMsg $ "New mint request received:" .< tokens
    ref <- getRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> tokens))

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
    let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
        txs = [testMintTx $ map unToken ts]
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

execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs