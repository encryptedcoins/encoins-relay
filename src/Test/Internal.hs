{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Internal where

import           Control.Monad            (unless, forM_)
import           Control.Monad.IO.Class   (MonadIO(..))
import           Control.Monad.Reader     (MonadReader, ReaderT(..), asks)
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromJust)
import qualified Data.Text.IO             as T
import           IO.Wallet                (HasWallet(..), getWalletAddr, ownAddresses)
import           Ledger                   (Address)
import           Server.Endpoints.Balance (getBalance, Balance(..))
import           Server.Internal          (HasServer(getCurrencySymbol), Env(..), loadEnv)
import           Utils.Address            (bech32ToAddress)
import           Utils.Logger             (HasLogger(..))

testBalance :: forall s. HasServer s => IO ()
testBalance = runTestM @s $ do
    addr <- getWalletAddr
    printBalance @s [addr]

testBalanceAll :: forall s. HasServer s => IO ()
testBalanceAll = runTestM @s $ getAddresses >>= printBalance
    where
        getAddresses = ownAddresses <&> map (fromJust . bech32ToAddress)

printBalance :: forall s. HasServer s => [Address] -> TestM s ()
printBalance addreses = do
    cs <- getCurrencySymbol
    forM_ addreses $ \addr -> do
        Balance b <- getBalance cs addr
        unless (null b) $ liftIO $ print b

newtype TestM s a = TestM { unTestM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env s))

runTestM :: forall s a. HasServer s => TestM s a -> IO a
runTestM tm = do
    env <- loadEnv
    runReaderT (unTestM tm) env

instance HasLogger (TestM s) where
    loggerFilePath = ""

    logMsg = liftIO . T.putStrLn

instance HasWallet (TestM s) where
    getRestoreWallet = asks envWallet