{-# LANGUAGE TypeApplications #-}

module Client.Opts where

import Control.Applicative             (some, (<|>))
import Control.Monad.Reader            (ask)
import Data.Maybe                      (fromMaybe)
import Data.String                     (IsString(..))
import Ledger.Ada                      (Ada(..))
import Options.Applicative             (Parser, (<**>), auto, fullDesc, help, info, long, option, short, value, execParser, 
                                        helper, Mod, OptionFields, flag')
import Options.Applicative.Types       (ReadM(..))

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser Options
optionsParser = Options <$> (autoModeParser <|> manualModeParser)

newtype Options = Options { mode :: ClientMode } deriving Show

data ClientMode 
    = Auto   AutoOptions
    | Manual ClientRequest
    deriving Show

--------------------------------------------- Auto ---------------------------------------------

-- Usage: --auto -i 30 -m 1

data AutoOptions = AutoOptions
    { averageRequestInterval :: Interval
    , maxTokensInReq         :: Maximum
    } deriving Show

type Interval = Int
type Maximum  = Int

autoModeParser :: Parser ClientMode
autoModeParser 
    = fmap Auto $ ((flag' AutoOptions (long "auto"))) 
    <*> intervalParser 
    <*> maxTokensParser

intervalParser :: Parser Interval
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average client request interval in seconds."
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

-- Usage: --manual --mint 154 --mint 16 --burn 13af.json

type ClientRequest = [RequestPiece]

data RequestPiece
    = RPMint Ada
    | RPBurn FilePath
    deriving (Show, Eq)

manualModeParser :: Parser ClientMode
manualModeParser = flag' Manual (long "manual")
               <*> some (mintParser <|> burnParser)

mintParser :: Parser RequestPiece
mintParser = RPMint . Lovelace <$> option auto
    (  long "mint"
    <> help "Token name to mint."
    )

burnParser :: Parser RequestPiece
burnParser = RPBurn <$> option withoutQuotes
    (  long "burn"
    <> help "Token name to burn."
    )

withoutQuotes :: ReadM String
withoutQuotes = ReadM $ ask >>= pure