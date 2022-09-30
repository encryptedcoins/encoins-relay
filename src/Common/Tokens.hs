{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Tokens where

import           Data.Aeson       (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.ByteString  as BS
import           Data.String      (IsString)
import           PlutusTx.Prelude (BuiltinByteString, toBuiltin, fromBuiltin)

newtype Token = Token
    { unToken :: BuiltinByteString
    } deriving newtype (Show, IsString, Eq)

type Tokens = [Token]

instance FromJSON Token where
    parseJSON = fmap (Token . toBuiltin . BS.pack) . parseJSON

instance ToJSON Token where
    toJSON = toJSON . BS.unpack . fromBuiltin . unToken