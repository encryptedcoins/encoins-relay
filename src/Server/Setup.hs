{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE RecordWildCards            #-}

module Server.Setup where

import           Utils.Logger           (HasLogger(..))
import           Control.Monad.IO.Class (MonadIO)
import           ENCOINS.Core.OffChain  (beaconMintTx, beaconSendTx)
import           Server.Internal        (Config(..), loadRestoreWallet)
import           Server.Tx              (mkTx)
import           IO.Wallet              (HasWallet(..))

newtype SetupM a = SetupM { unSetupM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger SetupM where
    loggerFilePath = "server.log"

instance HasWallet SetupM where
    getRestoreWallet = loadRestoreWallet

setupServer :: Config -> IO ()
setupServer Config{..} = unSetupM $ mkTx
    [ beaconMintTx confBeaconTxOutRef
    , beaconSendTx confBeaconTxOutRef
    ]