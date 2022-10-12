{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Internal where

import Common.Logger          (HasLogger(..))
import Common.Tokens          (Tokens)
import Control.Monad.Catch    (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (ReaderT(ReaderT), MonadReader, asks)
import Data.IORef             (IORef)
import Data.Sequence          (Seq)
import IO.Wallet              (HasWallet(..))
import Servant                (Handler)
import Server.Config          (restoreWalletFromConf)

newtype AppM a = AppM { unAppM :: ReaderT Env Handler a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadThrow, MonadCatch)

instance HasLogger AppM where
    loggerFilePath = "server.log"

instance HasWallet AppM where
    getRestoreWallet = restoreWalletFromConf

type Queue = Seq Tokens

type Ref = IORef Queue

newtype Env = Env
    { envRef :: Ref
    }

getRef :: AppM Ref
getRef = asks envRef