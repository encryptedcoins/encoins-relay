{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Encoins.Relay.Server.UtxosSpec where

import           Cardano.Server.Client.Gen      (randomAddressBech32Text)
import           Cardano.Server.Client.Client   (HasServantClientEnv)
import           Cardano.Server.Config          (decodeOrErrorFromFile, Config (..))
import           Cardano.Server.Endpoints.Utxos (UtxosApi)
import           Cardano.Server.Test.Utils      
import           Control.Monad.IO.Class         (MonadIO (..))
import           Encoins.Relay.Server.Config    (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Server    (EncoinsApi)
import           Servant                        (Proxy (..))
import           Servant.Client                 (client)
import           Test.Hspec                     (Spec, describe, it)

spec :: HasServantClientEnv => Spec
spec = describe "/utxos" $ do

    it "get utxos when all is ok" $ do
        Config{cAuxilaryConfig = EncoinsRelayConfig{..}, ..}
            <- decodeOrErrorFromFile @(Config EncoinsApi) "encoins-relay-test/test/configuration/config.json"
        addr <- randomAddressBech32Text cNetworkId
        shouldBeOk $ client (Proxy @UtxosApi) addr

    it "fails with malformed request body" $ do
        Malformed addrTxt <- liftIO malformedAddressTxt
        client (Proxy @UtxosApi) addrTxt `shoudlFailWithStatus` 400