module Server.Opts where

import Control.Applicative
import Options.Applicative

data ServerMode = ServerRun | ServerSetup

data Options = Options
    { serverMode :: ServerMode
    }

modeParser :: Parser ServerMode
modeParser =  subparser
    (  command "run"   (info (pure ServerRun  ) runDesc)
    <> command "setup" (info (pure ServerSetup) setupDesc)
    )
    where
        runDesc   = progDesc "Default server mode without scripts pre-hosting."
        setupDesc = progDesc "Server mode with placement of scripts on the blockchain."

optionsParser :: Parser Options
optionsParser = Options 
            <$> modeParser

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc