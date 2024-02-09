module Encoins.Relay.Apps.Ipfs.Utility where

import           Data.Text (Text, pack)

toText :: Show a => a -> Text
toText = pack . show