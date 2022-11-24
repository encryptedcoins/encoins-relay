{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Server.Setup where
import           Control.Monad.IO.Class (MonadIO(..))
import           IO.Wallet              (HasWallet(..))
import           Server.Internal        (loadConfig, Config(..), HasServer)
import           Utils.Logger           (HasLogger(..))

newtype SetupM s a = SetupM { unSetupM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger (SetupM s) where
    loggerFilePath = "server.log"

instance HasServer s => HasWallet (SetupM s) where
    getRestoreWallet = liftIO $ confWallet <$> loadConfig @s