{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Apps.Poll.Config where

import           Cardano.Api                 (NetworkId (Mainnet))
import           Cardano.Node.Emulator       (SlotConfig)
import           Cardano.Server.Config       (decodeOrErrorFromFile)
import           Control.Applicative         ((<|>))
import           Control.Lens                ((<&>), (^?))
import           Data.Aeson                  (withObject, (.:))
import qualified Data.Aeson                  as J
import           Data.Aeson.Lens             (key)
import           Data.Aeson.Types            (parseEither)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Encoins.Relay.Apps.Internal (encoinsCS, encoinsTokenName, defaultSlotConfigFilePath)
import           GHC.Stack                   (HasCallStack)
import           Ledger                      (CurrencySymbol, Slot, TokenName)
import           PlutusAppsExtra.Utils.Time  (parseSlotConfig, parseSlotOrUtc, utcToSlot)

data PollConfig = PollConfig
    { pcCs                 :: CurrencySymbol
    , pcTokenName          :: TokenName
    , pcStart              :: Slot
    , pcFinish             :: Slot
    , pcNetworkId          :: NetworkId
    , pcCheckDatumPkh      :: Bool
    , pcSlotConfig         :: SlotConfig
    } deriving (Show)

getConfig :: HasCallStack => FilePath -> IO PollConfig
getConfig fp = do
    val <- decodeOrErrorFromFile fp
    let slotConfigFp = fromMaybe defaultSlotConfigFilePath $ val ^? key "slotConfigFilePath" >>= \case
            J.String scFp -> pure $ T.unpack scFp
            _             -> Nothing
    pcSlotConfig <- decodeOrErrorFromFile slotConfigFp >>= either error pure . parseEither parseSlotConfig
    pure $ either error id $ flip parseEither val $ withObject "PollConfig" $ \o -> do
        pcCs                 <- o .: "currencySymbol" <|> pure encoinsCS
        pcTokenName          <- o .: "tokenName" <|> pure encoinsTokenName
        pcStart              <- o .: "start"  >>= parseSlotOrUtc <&> either (utcToSlot pcSlotConfig) id
        pcFinish             <- o .: "finish" >>= parseSlotOrUtc <&> either (utcToSlot pcSlotConfig) id
        pcNetworkId          <- o .: "networkId" <|> pure Mainnet
        pcCheckDatumPkh      <- o .: "checkDatumPkh" <|> pure True
        pure $ PollConfig{..}