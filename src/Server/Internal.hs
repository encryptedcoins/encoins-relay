{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Server.Internal where

import Common.Logger                   (HasLogger(..))
import Control.Monad.Catch             (MonadThrow, MonadCatch)
import Control.Monad.IO.Class          (MonadIO)
import Control.Monad.Reader            (ReaderT(ReaderT), MonadReader, asks)
import Data.IORef                      (IORef)
import Data.Sequence                   (Seq)
import ENCOINS.Core.Bulletproofs.Types (Inputs)
import IO.Wallet                       (HasWallet(..), RestoreWallet)
import Ledger                          (TxOutRef)
import Servant                         (Handler)

newtype AppM a = AppM { unAppM :: ReaderT Env Handler a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , HasWallet
        , MonadThrow
        , MonadCatch
        )

instance HasLogger AppM where
    loggerFilePath = "server.log"

instance (Monad m, MonadIO m) => HasWallet (ReaderT Env m) where
    getRestoreWallet = asks envWallet

type Queue = Seq Inputs

type QueueRef = IORef Queue

data Env = Env
    { envQueueRef   :: QueueRef
    , envBeaconRef  :: TxOutRef
    , envWallet     :: RestoreWallet
    }

getQueueRef :: AppM QueueRef
getQueueRef = asks envQueueRef