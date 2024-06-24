{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Encoins.Relay.Client.Main where

import qualified CSL
import           Control.Concurrent                           (threadDelay)
import           Control.Monad                                (forever, void)
import           Control.Monad.Catch                          (throwM)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Control.Monad.Reader                         (MonadReader (..))
import           Data.Data                                    (Proxy (..))
import           Data.Text                                    (Text)
import           ENCOINS.Bulletproofs                         (BulletproofSetup)
import           Encoins.Relay.Client.Opts                    (ClientCommand (..), commandParser, genEncoinsMode, genStatusReqBody,
                                                               genUtxosReq)
import           Encoins.Relay.Client.Secrets                 (genTerms)
import           Encoins.Relay.Client.TxClient                (TxClientCosntraints, txClientRedeemer)
import           Encoins.Relay.Common                         (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Client                  (HasServantClientEnv, autoClientWith, autoPing, autoVersion, manualClient,
                                                               manualPing, manualVersion, pingClient, sendRequest, versionClient)
import           Encoins.Relay.Common.Client.Opts             (runWithOpts)
import           Encoins.Relay.Common.Error.Envelope          (EndpointEnvelope)
import           Encoins.Relay.Common.Server                  (mkServantClientEnv)
import           Encoins.Relay.Common.Server.Endpoint.Ping    (PingEndpoint)
import           Encoins.Relay.Common.Server.Endpoint.Version (VersionEndpoint)
import           Encoins.Relay.Common.Server.Handler          (GetEndpointName)
import           Encoins.Relay.Server.Config                  (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Endpoint.NewTx          (NewTxEndpoint)
import           Encoins.Relay.Server.Endpoint.ServerTx       (ServerTxEndpoint)
import           Encoins.Relay.Server.Endpoint.Status         (EncoinsStatusReqBody (..), StatusEndpoint)
import           Encoins.Relay.Server.Endpoint.SubmitTx       (SubmitTxEndpoint, SubmitTxReqBody (..))
import           Encoins.Relay.Server.Endpoint.Utxos          (UtxosEndpoint)
import           Encoins.Relay.Server.Internal                (EncoinsServerInput, EncoinsServerM, loadEncoinsRelayEnv, runEncoinsServerM)
import           Encoins.Relay.Server.Server                  ()
import           GHC.TypeLits                                 (KnownSymbol)
import           Servant.Client                               (ClientM, HasClient (..), client)
import           System.Directory                             (createDirectoryIfMissing)
import           System.Random                                (randomIO, randomRIO)

runEncoinsClient :: FilePath -> FilePath -> IO ()
runEncoinsClient configFp bulletproofSetupFp = do
    createDirectoryIfMissing True "secrets"
    config <- decodeOrErrorFromFile configFp
    bulletproofSetup <- decodeOrErrorFromFile bulletproofSetupFp
    env <- loadEncoinsRelayEnv config
    c <- runWithOpts commandParser
    clientEnv <- mkServantClientEnv (cPort config) (cHost config) (cHyperTextProtocol config)
    let ?servantClientEnv = clientEnv
        ?bulletproofSetup = bulletproofSetup
    runEncoinsServerM env $ runCommand c

runCommand ::
    ( ?bulletproofSetup :: BulletproofSetup
    , HasServantClientEnv
    ) => ClientCommand -> EncoinsServerM ()
runCommand = \case
    AutoPing i -> autoPing i
    AutoVersion i -> autoVersion i
    AutoUtxos i -> autoClientWith @UtxosEndpoint genUtxosReq i utxosClient
    AutoNewTxRedeemer i -> forever $ autoTxMonadicClient @NewTxEndpoint >> waitI i
    AutoServerTxRedeemer i -> forever $ autoTxMonadicClient @ServerTxEndpoint >> waitI i
    AutoSumbitTx i -> autoClientWith @SubmitTxEndpoint (error "AutoSubmitTx isn't implemented yet.") i submitTxClient
    AutoStatus i -> autoClientWith @StatusEndpoint genStatusReqBody i statusClient
    ManualPing -> void manualPing
    ManualVersion -> void manualVersion
    ManualUtxos addr -> void $ manualClient @UtxosEndpoint $ utxosClient addr
    ManualNewTxRedeemer mode input -> let ?mode = mode in void $ fst =<< txClientRedeemer @NewTxEndpoint input
    ManualServerTxRedeemer mode input -> let ?mode = mode in void $ fst =<< txClientRedeemer @ServerTxEndpoint input
    ManualSumbitTx tx -> void $ manualClient @SubmitTxEndpoint $ submitTxClient tx
    ManualStatus reqBody -> void $ manualClient @StatusEndpoint $ statusClient reqBody
    RandomEndpoint i -> forever $ do
        liftIO (randomRIO @Int (0, 5)) >>= \case
            0 -> void $ sendRequest @PingEndpoint pingClient
            1 -> void $ sendRequest @VersionEndpoint versionClient
            2 -> void $ sendRequest @UtxosEndpoint . utxosClient =<< genUtxosReq
            3 -> void $ autoTxMonadicClient @NewTxEndpoint
            4 -> void $ autoTxMonadicClient @ServerTxEndpoint
            5 -> void $ sendRequest @StatusEndpoint . statusClient =<< randomIO
            _ -> void $ error "AutoSubmitTx isn't implemented yet."
        waitI i
  where
    waitI i = do
        d <- randomRIO (1, fromEnum i * 2)
        liftIO $ threadDelay d

-- Clients

autoTxMonadicClient :: forall e res.
    ( ?bulletproofSetup :: BulletproofSetup
    -- , ClientConstraints e m
    , TxClientCosntraints e res
    , Client ClientM e ~ ((EncoinsServerInput, CSL.TransactionInputs) -> ClientM res)
    , EndpointEnvelope e ~ res
    , KnownSymbol (GetEndpointName e)
    -- , HasServantClientEnv
    -- , HasClient ClientM e
    -- , Show res
    -- , EndpointEnvelope e ~ res
    -- , MonadIO m
    -- , MonadCatch m
    ) => EncoinsServerM res
autoTxMonadicClient = do
    env <- ask
    mode <- genEncoinsMode
    let ?mode = mode
    terms <- genTerms
    sendRequest @e $ liftIO $ runEncoinsServerM env $ do
        (a, res) <- txClientRedeemer @e terms
        a
        either throwM pure res

utxosClient :: Text -> ClientM (EndpointEnvelope UtxosEndpoint)
utxosClient = client (Proxy @UtxosEndpoint)

newTxClient :: (EncoinsServerInput, CSL.TransactionInputs) -> ClientM (EndpointEnvelope NewTxEndpoint)
newTxClient = client (Proxy @NewTxEndpoint)

submitTxClient :: SubmitTxReqBody -> ClientM (EndpointEnvelope SubmitTxEndpoint)
submitTxClient = client (Proxy @SubmitTxEndpoint)

statusClient :: EncoinsStatusReqBody -> ClientM (EndpointEnvelope StatusEndpoint)
statusClient = client (Proxy @StatusEndpoint)
