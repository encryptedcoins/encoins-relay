{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Encoins.Relay.Poll.Config where

import           Cardano.Api           (NetworkId (Mainnet))
import           Cardano.Node.Emulator (posixTimeToEnclosingSlot, utcTimeToPOSIXTime)
import           Control.Applicative   (Alternative, (<|>))
import           Data.Aeson            (FromJSON (..), withObject, (.:))
import           Data.Default          (Default (..))
import           Data.Foldable         (asum)
import           Data.Time             (UTCTime, defaultTimeLocale, parseTimeM)
import           Ledger                (Slot)
import           Plutus.V2.Ledger.Api  (CurrencySymbol, TokenName)

data PollConfig = PollConfig
    { pcCS :: CurrencySymbol
    , pcTokenName :: TokenName
    , pcStart :: Slot
    , pcFinish :: Slot
    , pcNetworkId :: NetworkId
    } deriving (Show)

instance FromJSON PollConfig where
    parseJSON = withObject "PollConfig" $ \o -> do
        pcCS        <- o .: "currencySymbol" <|> pure encoinsCS
        pcTokenName <- o .: "tokenName" <|> pure encoinsTokenName
        pcStart     <- o .: "start" >>= parseTime
        pcFinish    <- o .: "finish" >>= parseTime
        pcNetworkId <- o .: "networkId" <|> pure Mainnet
        pure $ PollConfig{..}

parseTime :: (Alternative m, MonadFail m) => String -> m Slot
parseTime s = fmap utcToSlot $ asum $ (\f -> parseTimeM True defaultTimeLocale f s)
    <$> ["%Y-%m-%d", "%Y-%m-%d-%H", "%Y-%m-%d-%H:%M"]

utcToSlot :: UTCTime -> Slot
utcToSlot = (+ 4492827) . posixTimeToEnclosingSlot def . utcTimeToPOSIXTime

encoinsTokenName :: TokenName
encoinsTokenName = "ENCS"

encoinsCS :: CurrencySymbol
encoinsCS = "9abf0afd2f236a19f2842d502d0450cbcd9c79f123a9708f96fd9b96"