module Main where

import Cloud.Spec

import Test.Hspec (hspec)


main :: IO ()
main = hspec $ do
  getNotDiscardedSpec
  isDiscardedSpec
  isDiscardedInListSpec