{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Server.Internal where

import           Control.Monad.Catch    (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ReaderT(ReaderT), MonadReader, asks)
import           Data.Aeson             (FromJSON(..), ToJSON)
import           Data.IORef             (IORef, newIORef)
import           Data.Kind              (Type)
import           Data.Sequence          (Seq, empty)
import           IO.Wallet              (HasWallet(..), RestoreWallet)
import           Ledger                 (CurrencySymbol)
import           Servant                (Handler, MimeUnrender, JSON)
import           Server.Config          (Config(..), configFile, decodeOrErrorFromFile)
import           Utils.Logger           (HasLogger(..))

class ( Show (AuxiliaryEnvOf s)
      , FromJSON (AuxiliaryEnvOf s)
      , MimeUnrender JSON (RedeemerOf s)
      , ToJSON (RedeemerOf s)
      , Show (RedeemerOf s)
      ) => HasServer s where

    type AuxiliaryEnvOf s :: Type

    loadAuxiliaryEnv :: FilePath -> IO (AuxiliaryEnvOf s)

    setupServer :: (MonadReader (Env s) m, HasLogger m, HasWallet m) => m ()

    type RedeemerOf s :: Type

    getCurrencySymbol :: MonadReader (Env s) m => m CurrencySymbol

    processTokens :: (MonadReader (Env s) m, HasWallet m, HasLogger m) => RedeemerOf s -> m ()

newtype AppM s a = AppM { unAppM :: ReaderT (Env s) Handler a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , HasWallet
        , MonadThrow
        , MonadCatch
        )

instance HasLogger (AppM s) where
    loggerFilePath = "server.log"

instance (Monad m, MonadIO m) => HasWallet (ReaderT (Env s) m) where
    getRestoreWallet = asks envWallet

type Queue s = Seq (RedeemerOf s)

type QueueRef s = IORef (Queue s)

data Env s = Env
    { envQueueRef       :: QueueRef s
    , envWallet         :: RestoreWallet
    , envAuxiliary      :: AuxiliaryEnvOf s
    , envMinUtxosAmount :: Int
    }

getQueueRef :: AppM s (QueueRef s)
getQueueRef = asks envQueueRef

loadEnv :: forall s. HasServer s => IO (Env s)
loadEnv = do
    Config{..} <- decodeOrErrorFromFile configFile
    let envMinUtxosAmount = cMinUtxosAmount
    envQueueRef  <- newIORef empty
    envWallet    <- decodeOrErrorFromFile cWalletFile
    envAuxiliary <- loadAuxiliaryEnv @s cAuxiliaryEnvFile
    pure Env{..}