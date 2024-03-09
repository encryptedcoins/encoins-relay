{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Encoins.Relay.Server.Version where

import           Cardano.Server.Endpoints.Version (ServerVersion (..), parseSvDate, versionEndpointHandler)
import           Cardano.Server.Internal          (ServerM)
import           Development.GitRev               (gitCommitDate, gitHash)
import           Paths_encoins_relay_server       (version)

versionHandler :: ServerM api ServerVersion
versionHandler = versionEndpointHandler version $(gitHash) $(gitCommitDate)

relayVersion :: ServerVersion
relayVersion = ServerVersion version $(gitHash) (parseSvDate $(gitCommitDate))