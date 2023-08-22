{-# LANGUAGE LambdaCase  #-}

module Main where

import           Encoins.Relay.Apps.Poll.Main (poll)
import           System.Environment           (getArgs)
import           Text.Read                    (readEither)

main :: IO ()
main = getArgs >>= \case
    pollNo:_ -> either (error "Incorrect poll number.") poll $ readEither pollNo
    _        -> error "No poll number is given."
