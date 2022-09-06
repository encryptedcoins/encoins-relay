{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Tokens where

import Data.Aeson

newtype Token = Token 
    { unToken :: Int
    } deriving newtype (Show, Num, Enum, FromJSON, ToJSON)

type Tokens = [Token]