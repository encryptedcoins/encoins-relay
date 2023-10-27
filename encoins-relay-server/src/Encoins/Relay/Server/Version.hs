{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.Version where

import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           Data.Text                     (Text, pack)
import qualified Data.Text                     as T
import           Cardano.Server.Internal       (ServerM)
import           Data.Time                     (UTCTime, defaultTimeLocale,
                                                formatTime, parseTimeOrError)
import           Data.Version                  (Version, showVersion)
import           Development.GitRev            (gitCommitDate, gitHash)
import           GHC.Generics                  (Generic)
import           Paths_encoins_relay_server    (version)
import           Prettyprinter                 (Pretty (pretty), annotate,
                                                defaultLayoutOptions,
                                                layoutSmart)
import           Prettyprinter.Render.Terminal (Color (Blue, Green), bold,
                                                color, renderStrict)

data ServerVersion = ServerVersion
  { svVersion :: Version
  , svCommit  :: Text
  , svDate    :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

relayVersion :: ServerVersion
relayVersion = ServerVersion
  { svVersion = version
  , svCommit = $(gitHash)
  , svDate = parseTimeOrError
      False
      defaultTimeLocale
      "%a %b %e %T %Y %Z"
      $(gitCommitDate)
  }

showRelayVersion :: ServerVersion -> String
showRelayVersion sv = T.unpack $ T.intercalate "\n" $ [sVersion, sHash, sDate]
    where
        sVersion = textToColorText green $ "Encoins-relay-server " <> "v" <> T.pack (showVersion $ svVersion sv)
        sHash = " ➤ " <> (textToColorText blue ("Git revision: " :: T.Text)) <> svCommit sv
        sDate = " ➤ " <> (textToColorText blue ("Commit date:  " :: T.Text)) <> dateFormated
        dateFormated = formatPollTime $ svDate sv
        textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt
        green = annotate $ color Green <> bold
        blue = annotate $ color Blue <> bold

formatPollTime :: UTCTime -> Text
formatPollTime
  = pack
  . formatTime defaultTimeLocale "%e %B %Y, %R %Z"

versionHandler :: ServerM api ServerVersion
versionHandler = pure relayVersion
