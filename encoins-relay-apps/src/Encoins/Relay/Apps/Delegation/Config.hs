{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Apps.Delegation.Config where

import           Cardano.Api                 (NetworkId (Mainnet))
import           Cardano.Node.Emulator       (SlotConfig)
import           Cardano.Server.Config       (decodeOrErrorFromFile)
import           Control.Applicative         ((<|>))
import           Control.Lens                ((<&>), (^?))
import           Data.Aeson                  (withObject, (.:))
import           Data.Aeson.Lens             (key)
import           Data.Aeson.Types            (parseEither)
import qualified Data.Aeson.Types            as J
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Encoins.Relay.Apps.Internal (encoinsCS, encoinsTokenName)
import           GHC.Generics                (Generic)
import           GHC.Stack                   (HasCallStack)
import           Ledger                      (CurrencySymbol, Slot, TokenName)
import           PlutusAppsExtra.Utils.Time  (parseSlotConfig, parseSlotOrUtc, utcToSlot)

data DelegationConfig = DelegationConfig
    { dcNetworkId          :: NetworkId
    , dcMinTokenAmount     :: Integer
    , dcDelegationStart    :: Slot
    , dcCs                 :: CurrencySymbol
    , dcTokenName          :: TokenName
    , dcSlotConfig         :: SlotConfig
    } deriving (Show, Eq, Generic)

getConfig :: HasCallStack => FilePath -> IO DelegationConfig
getConfig fp = do
    val <- decodeOrErrorFromFile fp
    let slotConfigFp = fromMaybe (error "No slot config file path.") $ val ^? key "slotConfigFilePath" >>= \case
            J.String scFp -> pure $ T.unpack scFp
            _             -> Nothing
    dcSlotConfig <- decodeOrErrorFromFile slotConfigFp >>= either error pure . parseEither parseSlotConfig
    pure $ either error id $ flip parseEither val $ withObject "DelegationConfig" $ \o -> do
        dcNetworkId          <- o .: "networkId" <|> pure Mainnet
        dcMinTokenAmount     <- o .: "minTokenAmount"
        dcDelegationStart    <- o .: "delegationStart" >>= parseSlotOrUtc <&> either (utcToSlot dcSlotConfig) id
        dcCs                 <- o .: "currencySymbol" <|> pure encoinsCS
        dcTokenName          <- o .: "tokenName" <|> pure encoinsTokenName
        pure $ DelegationConfig{..}