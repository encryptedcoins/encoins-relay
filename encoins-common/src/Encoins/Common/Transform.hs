{-# LANGUAGE OverloadedStrings  #-}

module Encoins.Common.Transform where

import           Data.Text (Text, pack)


toText :: Show a => a -> Text
toText = pack . show

space :: Text
space = " "