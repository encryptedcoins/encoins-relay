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
import           Data.Aeson             (FromJSON(..), ToJSON, eitherDecode, genericParseJSON)
import           Data.Aeson.Casing      (aesonPrefix, snakeCase)
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (fromStrict)
import           Data.IORef             (IORef)
import           Data.Kind              (Type)
import           Data.Sequence          (Seq)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           IO.Wallet              (HasWallet(..), RestoreWallet)
import           Ledger                 (CurrencySymbol)
import           Servant                (Handler, MimeUnrender, JSON)
import           Utils.Logger           (HasLogger(..))

class ( Show (AuxiliaryEnvOf s)
      , FromJSON (AuxiliaryEnvOf s)
      , MimeUnrender JSON (RedeemerOf s)
      , ToJSON (RedeemerOf s)
      , Show (RedeemerOf s)
      ) => HasServer s where

    type AuxiliaryEnvOf s :: Type

    loadAuxiliaryEnv :: FilePath -> IO (AuxiliaryEnvOf s)

    setupServer :: (HasLogger m, HasWallet m) => Config s -> m ()

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
    { envQueueRef  :: QueueRef s
    , envWallet    :: RestoreWallet
    , envAuxiliary :: AuxiliaryEnvOf s
    }

getQueueRef :: AppM s (QueueRef s)
getQueueRef = asks envQueueRef

data Config s = Config
    { confServerAddress     :: Text
    , confNodeAddress       :: Text
    , confChainIndexAddress :: Text
    , confAuxiliaryEnv      :: AuxiliaryEnvOf s
    , confWallet            :: RestoreWallet
    }
deriving instance HasServer s => Show (Config s)

data ConfigFile = ConfigFile
    { cfServerAddress     :: Text
    , cfNodeAddress       :: Text
    , cfChainIndexAddress :: Text
    , cfAuxiliaryEnvFile  :: FilePath
    , cfWalletFile        :: FilePath
    } deriving (Show, Generic)

instance FromJSON ConfigFile where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

loadConfig :: forall s. HasServer s => IO (Config s)
loadConfig = do
    ConfigFile{..} <- decodeOrErrorFromFile "testnet/config.json"
    let confServerAddress     = cfServerAddress
        confNodeAddress       = cfNodeAddress
        confChainIndexAddress = cfChainIndexAddress
    confAuxiliaryEnv <- loadAuxiliaryEnv @s cfAuxiliaryEnvFile
    confWallet       <- decodeOrErrorFromFile cfWalletFile
    pure Config{..}

decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
decodeOrErrorFromFile =  fmap (either error id . eitherDecode . fromStrict) . BS.readFile