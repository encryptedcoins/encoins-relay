{-# LANGUAGE NumericUnderscores #-}

module Encoins.Relay.Server.Opts where

import           Data.Foldable       (asum)
import           Ledger              (Ada)
import           Options.Applicative (Parser, auto, execParser, flag', fullDesc, help, helper, info, long, metavar, option,
                                      (<**>))

runWithOpts :: IO ServerMode
runWithOpts = execParser $ info (modeParser <**> helper) fullDesc

data ServerMode = Run | Setup | Reward Ada

modeParser :: Parser ServerMode
modeParser = asum [runMode, setupMode, rewardMode, pure Run]

runMode :: Parser ServerMode
runMode = flag' Run
    (  long "run"
    <> help "Default server mode without any preliminary work.")

setupMode :: Parser ServerMode
setupMode = flag' Setup
    (  long "setup"
    <> help "Mint and send ENCOINS beacon token.")

rewardMode :: Parser ServerMode
rewardMode = Reward . fromInteger . (* 1_000_000) <$> option auto
    (  long    "reward"
    <> help    "Distribute delegation rewards."
    <> metavar "ADA"
    )