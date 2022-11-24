{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Server.Internal where

import           Control.Monad.Catch             (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Control.Monad.Reader            (ReaderT(ReaderT), MonadReader, asks) 
import           Data.Aeson                      (FromJSON (parseJSON), eitherDecode, genericParseJSON)
import           Data.Aeson.Casing               (aesonPrefix, snakeCase)
import qualified Data.ByteString                 as BS
import           Data.ByteString.Lazy            (fromStrict)
import           Data.IORef                      (IORef)
import           Data.Sequence                   (Seq)
import           Data.Text                       (Text)
import           ENCOINS.Core.OnChain            (EncoinsRedeemer)
import           GHC.Generics                    (Generic)
import           GHC.TypeNats                    (Nat)
import           IO.Wallet                       (HasWallet(..), RestoreWallet)
import           Ledger                          (TxOutRef)
import           Servant                         (Handler, WithStatus(..), Union, IsMember, respond)
import           Servant.API.Status              (KnownStatus)
import           Utils.Logger                    (HasLogger(..))

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


data Config = Config
    { confServerAddress     :: Text
    , confNodeAddress       :: Text
    , confChainIndexAddress :: Text
    , confBeaconTxOutRef    :: TxOutRef
    , confWallet            :: RestoreWallet
    } deriving (Show)

data ConfigFile = ConfigFile
    { cfServerAddress          :: Text
    , cfNodeAddress            :: Text
    , cfChainIndexAddress      :: Text
    , cfAdaStakingTxOutRefFile :: FilePath
    , cfWalletFile             :: FilePath
    } deriving (Show, Generic)

instance FromJSON ConfigFile where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

loadConfig :: IO Config
loadConfig = do
    ConfigFile{..} <- decodeOrErrorFromFile "testnet/config.json"
    let confServerAddress     = cfServerAddress
        confNodeAddress       = cfNodeAddress
        confChainIndexAddress = cfChainIndexAddress
    confBeaconTxOutRef <- decodeOrErrorFromFile cfAdaStakingTxOutRefFile
    confWallet         <- decodeOrErrorFromFile cfWalletFile
    pure Config{..}
  where
    decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a 
    decodeOrErrorFromFile =  fmap (either error id . eitherDecode  . fromStrict) . BS.readFile 

loadRestoreWallet :: MonadIO m => m RestoreWallet
loadRestoreWallet = liftIO $ confWallet <$> loadConfig