{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Client.Opts where

import           Client.Internal       (HasClient(..))
import           Control.Applicative   (some, (<|>))
import           Options.Applicative   (Parser, (<**>), auto, fullDesc, help, info, long, option, short, value, execParser,
                                        helper, flag')

runWithOpts :: HasClient s => IO (Options s)
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: HasClient s => Parser (Options s)
optionsParser = autoModeParser <|> manualModeParser

data Options s
    = Auto   AutoOptions
    | Manual [RequestPieceOf s]
deriving instance HasClient s => Show (Options s)

--------------------------------------------- Auto ---------------------------------------------

data AutoOptions = AutoOptions
    { averageRequestInterval :: Interval
    , maxTokensInReq         :: Maximum
    } deriving Show

type Interval = Int
type Maximum  = Int

autoModeParser :: Parser (Options s)
autoModeParser
    = fmap Auto $ flag' AutoOptions (long "auto")
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
    (  long  "max"
    <> short 'm'
    <> help  "Upper bound on the number of generated tokens in a single request."
    <> value 1
    )

-------------------------------------------- Manual --------------------------------------------

manualModeParser :: forall s. HasClient s => Parser (Options s)
manualModeParser = flag' Manual (long "manual")
               <*> some (parseRequestPiece @s)