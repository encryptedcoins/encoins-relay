module Server.Opts where

import Options.Applicative (Parser, (<**>), command, fullDesc, info, progDesc, subparser, execParser, helper)

data ServerType = Encoins | Test
data ServerMode = Run | Setup

data Options = Options
    { serverType :: ServerType
    , serverMode :: ServerMode
    }

typeParser :: Parser ServerType
typeParser = subparser
        (  command "encoins" (info (pure Encoins) encoinsDesc)
        <> command "test"    (info (pure Test)    testDesc)
        )
    where
        encoinsDesc = progDesc "The app will work with encoins tokens."
        testDesc    = progDesc "The app will work with test tokens."

modeParser :: Parser ServerMode
modeParser = subparser
        (  command "run"   (info (pure Run  ) runDesc)
        <> command "setup" (info (pure Setup) setupDesc)
        )
    where
        runDesc   = progDesc "Default server mode without any preliminary work."
        setupDesc = progDesc "Do some preliminary work before starting the server."

optionsParser :: Parser Options
optionsParser = Options 
            <$> typeParser
            <*> modeParser

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc