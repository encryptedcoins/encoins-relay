{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Internal where

import Common.Logger          (HasLogger(..))
import Common.Tokens          (Tokens)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (ReaderT(ReaderT), MonadReader, asks)
import Data.IORef             (IORef)
import Data.Sequence          (Seq)
import Servant                (Handler)

newtype AppM a = AppM { unAppM :: ReaderT Env Handler a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance HasLogger AppM where
    loggerFilePath = "server.log"

type Queue = Seq Tokens

type Ref = IORef Queue

newtype Env = Env
    { envRef :: Ref
    }

getRef :: AppM Ref
getRef = asks envRef