{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Client.Opts where

import           Cardano.Server.Client.Internal (Mode, ServerEndpoint)
import           Cardano.Server.Client.Opts     (CommonOptions (..), autoModeParser, manualModeParser, serverEndpointParser)
import           Control.Applicative            ((<|>), liftA3)
import           Data.Aeson                     (ToJSON)
import           Data.Bool                      (bool)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           ENCOINS.Bulletproofs.Types     (Secret (..))
import           ENCOINS.Core.V1.OffChain       (EncoinsMode (..))
import           GHC.Generics                   (Generic)
import           Ledger.Ada                     (Ada (..))
import           Options.Applicative            (Parser, argument, auto, execParser, fullDesc, helper, info, metavar, value,
                                                 (<**>))
import           System.Random                  (Random (..))
import           Text.Read                      (readMaybe)

data Options = Options
    { optsEndpoint    :: ServerEndpoint
    , optsMode        :: Mode
    , optsEncoinsMode :: EncoinsMode
    } deriving (Show, Eq)

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser Options
optionsParser = liftA3 Options 
    serverEndpointParser 
    (autoModeParser <|> manualModeParser) 
    encoinsModeParser

encoinsModeParser :: Parser EncoinsMode
encoinsModeParser = argument auto
    (  value WalletMode
    <> metavar "WalletMode | LedgerMode"
    )

extractCommonOptions :: Options -> CommonOptions
extractCommonOptions Options{..} = CommonOptions optsEndpoint optsMode

data EncoinsRequestTerm
    = RPMint Ada
    | RPBurn (Either Secret FilePath)
    deriving (Show, Eq, Generic, ToJSON)

instance Random EncoinsRequestTerm where
    random g = 
        let (b, g')   = random g
            (a, g'')  = random g'
            (s, g''') = random g''
        in bool (RPMint $ Lovelace a, g'') (RPBurn $ Left s, g''') b
    randomR _ = random

instance Random [EncoinsRequestTerm] where
    random g  =
        let (reqTerms, gNew) = foldr (\_ (lst, g') -> let (e, g'') = random g' in (e:lst, g'')) ([], g) [1 :: Integer .. 5]
        in (reqTerms, gNew)
    randomR _ = random

readTerms :: Text -> Maybe [EncoinsRequestTerm]
readTerms = mapM (readTerm . T.unpack) . T.splitOn ","
    where
        readTerm = \case
            'b':fp  -> Just $ RPBurn $ Right fp
            'm':ada -> fmap (RPMint . fromInteger) . readMaybe $ ada
            _       -> Nothing