{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Encoins.Relay.Common.Diagnostic where

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.Async                  (async, wait)
import           Control.Monad                             (forever)
import           Control.Monad.Catch                       (Exception, MonadCatch, MonadThrow (throwM), SomeException, handle)
import           Control.Monad.Extra                       (whenM)
import           Control.Monad.IO.Class                    (MonadIO (..))
import           Data.Data                                 (Proxy (..))
import           Data.Text                                 (Text)
import           Encoins.Common.Log                        (logInfo)
import           Encoins.Common.Transform                  (toText)
import           Encoins.Relay.Common.Server.Endpoint.Ping (PingEndpoint)
import           Katip                                     (KatipContext)
import           PlutusAppsExtra                           (nodeHealthCheck)
import qualified PlutusAppsExtra.Api.Kupo                  as Kupo
import           PlutusAppsExtra.IO.ChainIndex             (HasChainIndexProvider (getChainIndexProvider))
import qualified PlutusAppsExtra.IO.ChainIndex             as ChainIndex
import           PlutusAppsExtra.IO.Tx                     (HasTxProvider (getTxProvider))
import qualified PlutusAppsExtra.IO.Tx                     as Tx
import qualified PlutusAppsExtra.Utils.Kupo                as Kupo
import qualified Servant.Client                            as Servant
import           Servant.Client                            (ClientM)

doDiagnostics :: MonadIO m => Int -> m () -> m ()
doDiagnostics i d = forever $ do
    delay <- liftIO $ async $ threadDelay i
    d
    liftIO $ wait delay

providersDiagnostics :: (MonadCatch m, KatipContext m, HasTxProvider m) => m ()
providersDiagnostics = do
    whenM (Tx.isCardanoTxProvider <$> getTxProvider)
        $ withDiagnostics "cardano-node" nodeDiagnostics
    whenM ((== ChainIndex.Kupo) <$> getChainIndexProvider)
        $ withDiagnostics "kupo" kupoDiagnostics

pingDiagnostics :: (MonadCatch m, KatipContext m) => Servant.ClientEnv -> m ()
pingDiagnostics env = clientDiagnostics env (Servant.client (Proxy @PingEndpoint))

clientDiagnostics :: (MonadCatch m, KatipContext m)
    => Servant.ClientEnv -> ClientM a -> m ()
clientDiagnostics env client = withDiagnostics "server" $ do
    res <- liftIO $ Servant.runClientM client env
    either throwM (const $ logInfo "Server is alive.") res

kupoDiagnostics :: (MonadCatch m, KatipContext m, HasChainIndexProvider m) => m ()
kupoDiagnostics = whenM ((== ChainIndex.Kupo) <$> getChainIndexProvider) $ withDiagnostics "kupo" $ do
    response <- liftIO Kupo.getHealth
    if Kupo.ghrConnected response
    then logInfo "Kupo is alive."
    else logInfo $ "Kupo isn't alive. Received response: " <> toText response

nodeDiagnostics :: (MonadCatch m, KatipContext m, HasTxProvider m) => m ()
nodeDiagnostics = whenM (Tx.isCardanoTxProvider <$> getTxProvider) nodeDiagnosticsUnwrapped

nodeDiagnosticsUnwrapped :: (MonadCatch m, KatipContext m) => m ()
nodeDiagnosticsUnwrapped = withDiagnostics "cardano-node" $ do
    _ <- liftIO nodeHealthCheck
    logInfo "Cardano-node is alive."

withDiagnostics :: (MonadCatch m, KatipContext m) => Text -> m () -> m ()
withDiagnostics serviceName diagnostics = handle h $ do
    logInfo $ "Checking if " <> serviceName <> " is alive..."
    diagnostics
  where
    h (e :: SomeException) = logInfo $ serviceName <> " isn't alive. Reason: " <> toText e

newtype DiagnosticsError = DiagnosticsError {getDiagnosticsError :: Text}
    deriving newtype  Show
    deriving anyclass Exception
