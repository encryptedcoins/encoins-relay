{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Cloud.DB.Discarded (testDiscarded)
import           Cloud.DB.Encoins (testEncoins)
import           Cloud.DB.Utility
import           Cloud.Spec

import           Test.Hspec         (hspec)


main :: IO ()
main = do
  hspec $ do
    getNotDiscardedSpec
    isDiscardedSpec
    isDiscardedInListSpec
  testEncoins `nextTest` testDiscarded
