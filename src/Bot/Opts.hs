module Bot.Opts where

import Options.Applicative

type Inteval = Int

data Options = Options
  { averageRequestInterval :: Inteval
  , maxTokensInReq :: Int
  }

intervalParser :: Parser Int
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average bot request interval in seconds."
    )

maxTokensParser :: Parser Int
maxTokensParser = option auto
    (  long "max"
    <> short 'm'
    <> help "Upper bound on the number of generated tokens in a request."
    <> value 20
    )

optionsParser :: Parser Options
optionsParser = Options <$> intervalParser <*> maxTokensParser

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc