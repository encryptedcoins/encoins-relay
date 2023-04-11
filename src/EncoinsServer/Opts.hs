{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module EncoinsServer.Opts where

import           Data.Aeson                 (ToJSON)
import           Data.Bool                  (bool)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           ENCOINS.Bulletproofs.Types (Secret (..))
import           GHC.Generics               (Generic)
import           Ledger.Ada                 (Ada (..))
import           Options.Applicative        (Alternative ((<|>)), Parser, command, execParser, fullDesc, helper, info, progDesc,
                                             subparser, (<**>))
import           System.Random              (Random (..))
import           Text.Read                  (readMaybe)

--------------------------------- Server ---------------------------------

runWithOpts :: IO ServerMode
runWithOpts = execParser $ info (modeParser <**> helper) fullDesc

data ServerMode = Run | Setup

modeParser :: Parser ServerMode
modeParser = (<|> pure Run) $ subparser
        (  command "run"   (info (pure Run  ) runDesc)
        <> command "setup" (info (pure Setup) setupDesc)
        )
    where
        runDesc   = progDesc "Default server mode without any preliminary work."
        setupDesc = progDesc "Mint and send ENCOINS beacon token."

--------------------------------- Client ---------------------------------

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