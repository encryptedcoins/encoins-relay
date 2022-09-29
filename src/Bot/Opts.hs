module Bot.Opts where

import Options.Applicative (Parser, (<**>), auto, fullDesc, help, info, long, option, short, value, execParser, helper)

type Inteval = Int

data Options = Options
    { averageRequestInterval :: Inteval
    , maxTokensInReq         :: Int
    }

intervalParser :: Parser Int
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average bot request interval in seconds."
    <> value 30
    )

maxTokensParser :: Parser Int
maxTokensParser = option auto
    (  long "max"
    <> short 'm'
    <> help "Upper bound on the number of generated tokens in a request."
    <> value 1
    )

optionsParser :: Parser Options
optionsParser = Options
            <$> intervalParser
            <*> maxTokensParser

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc