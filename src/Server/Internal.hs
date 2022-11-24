{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Server.Internal where

import Utils.Logger                    (HasLogger(..))
import Control.Monad.Catch             (MonadThrow, MonadCatch)
import Control.Monad.IO.Class          (MonadIO)
import Control.Monad.Reader            (ReaderT(ReaderT), MonadReader, asks)
import Data.IORef                      (IORef)
import Data.Sequence                   (Seq)
import Data.Text                       (Text)
import ENCOINS.Core.OnChain            (EncoinsRedeemer)
import GHC.TypeNats                    (Nat)
import IO.Wallet                       (HasWallet(..), RestoreWallet)
import Ledger                          (TxOutRef)
import Servant                         (Handler, WithStatus(..), Union, IsMember, respond)
import Servant.API.Status              (KnownStatus)

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

type Queue = Seq EncoinsRedeemer

type QueueRef = IORef Queue

data Env = Env
    { envQueueRef   :: QueueRef
    , envBeaconRef  :: TxOutRef
    , envWallet     :: RestoreWallet
    }

getQueueRef :: AppM QueueRef
getQueueRef = asks envQueueRef

respondWithStatus :: forall (s :: Nat) res. 
    ( IsMember (WithStatus s Text) res
    , KnownStatus s
    ) => Text -> AppM (Union res)
respondWithStatus msg = do
    logMsg msg
    respond (WithStatus @s msg)