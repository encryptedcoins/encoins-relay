module Encoins.Relay.Server.Opts where

import           Options.Applicative        (Alternative ((<|>)), Parser, command, execParser, fullDesc, helper, info, progDesc,
                                             subparser, (<**>))

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