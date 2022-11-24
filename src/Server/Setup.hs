{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Setup where
import           Control.Monad.Reader   (MonadIO, ReaderT(..), MonadReader, asks)
import           IO.Wallet              (HasWallet(..))
import           Server.Internal        (Env (..))
import           Utils.Logger           (HasLogger(..))

newtype SetupM s a = SetupM { unSetupM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env s))

runSetupM :: Env s -> SetupM s a -> IO a
runSetupM env = (`runReaderT` env) . unSetupM

instance HasLogger (SetupM s) where
    loggerFilePath = "server.log"

instance HasWallet (SetupM s) where
    getRestoreWallet = asks envWallet