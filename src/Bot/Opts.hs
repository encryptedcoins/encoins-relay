{-# LANGUAGE TypeApplications #-}

module Bot.Opts where

import Control.Applicative             (some, (<|>))
import Control.Monad.Reader            (ask)
import Data.Maybe                      (fromMaybe)
import Data.String                     (IsString(..))
import ENCOINS.Core.BaseTypes          (MintingPolarity(..), GroupElement, toGroupElement)
import ENCOINS.Core.Bulletproofs.Types (Inputs, Input(..))
import Options.Applicative             (Parser, (<**>), auto, fullDesc, help, info, long, option, short, value, execParser, 
                                        helper, Mod, OptionFields, flag')
import Options.Applicative.Types       (ReadM(..))

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser Options
optionsParser = Options <$> (autoModeParser <|> manualModeParser)

newtype Options = Options { mode :: BotMode } deriving Show

data BotMode 
    = Auto   AutoOptions
    | Manual Inputs 
    deriving Show

--------------------------------------------- Auto ---------------------------------------------

-- Usage: --auto -i 30 -m 1

data AutoOptions = AutoOptions
    { averageRequestInterval :: Interval
    , maxTokensInReq         :: Maximum
    } deriving Show

type Interval = Int
type Maximum  = Int

autoModeParser :: Parser BotMode
autoModeParser 
    = fmap Auto $ ((flag' AutoOptions (long "auto"))) 
    <*> intervalParser 
    <*> maxTokensParser

intervalParser :: Parser Interval
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average bot request interval in seconds."
    <> value 30
    )

maxTokensParser :: Parser Maximum
maxTokensParser = option auto
    (  long "max"
    <> short 'm'
    <> help "Upper bound on the number of generated tokens in a request."
    <> value 1
    )

-------------------------------------------- Manual --------------------------------------------

-- Usage: --manual --mint abc123 --mint ff22 --burn 4b917

manualModeParser :: Parser BotMode
manualModeParser = flag' Manual (long "manual")
               <*> some (mintParser <|> burnParser)

mintParser :: Parser Input
mintParser = mkInput Mint 
    (  long "mint"
    <> help "Token name to mint."
    )

burnParser :: Parser Input
burnParser = mkInput Burn
    (  long "burn"
    <> help "Token name to burn."
    )

mkInput :: MintingPolarity -> Mod OptionFields String -> Parser Input
mkInput polarity = fmap ((`Input` polarity) . parseGroupElement) . option withoutQuotes

parseGroupElement :: String -> GroupElement
parseGroupElement s = fromMaybe err $ toGroupElement $ fromString s
  where
    err = error $ "Incorrect token name: " <> s

withoutQuotes :: ReadM String
withoutQuotes = ReadM $ ask >>= pure