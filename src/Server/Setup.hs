{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}

module Server.Setup where

import           Common.Logger          (HasLogger(..))
import           Control.Monad.IO.Class (MonadIO)
import           ENCOINS.Core.OffChain  (beaconMintTx, beaconSendTx)
import           Server.Config          (loadConfig, Config(..), restoreWalletFromConf)
import           Server.ServerTx        (mkTxWithConstraints)
import           IO.Wallet              (HasWallet(..))

newtype SetupM a = SetupM { unSetupM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger SetupM where
    loggerFilePath = "server.log"

instance HasWallet SetupM where
    getRestoreWallet = restoreWalletFromConf

setupServer :: IO ()
setupServer = do
    utxoRef <- confAdaStakingTxOutRef <$> loadConfig
    unSetupM $ mkTxWithConstraints
        [ beaconMintTx utxoRef
        , beaconSendTx utxoRef
        ]