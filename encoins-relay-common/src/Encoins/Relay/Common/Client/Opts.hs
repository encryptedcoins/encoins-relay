{-# LANGUAGE LambdaCase #-}

module Encoins.Relay.Common.Client.Opts where

import           Options.Applicative (Alternative ((<|>)), CommandFields, Mod, Parser, auto, command, execParser, flag', fullDesc, help,
                                      helper, info, long, metavar, option, progDesc, short, value, (<**>))

runWithOpts :: Parser a -> IO a
runWithOpts p = execParser $ info (p <**> helper) fullDesc

pingCommand :: a -> (Interval -> a) -> Mod CommandFields a
pingCommand manualPing autoPing = let p = pingP manualPing autoPing
    in command "ping"    $ info p $ progDesc "Client for ping endpoint."

versionCommand :: a -> (Interval -> a) -> Mod CommandFields a
versionCommand manualVersion autoVersion = let v = versionP manualVersion autoVersion
    in command "version" $ info v $ progDesc "Client for version endpoint."

randomCommand :: (Interval -> a) -> Mod CommandFields a
randomCommand autoRandom = let r = randomP autoRandom
    in command "random"  $ info r $ progDesc "Random client."

pingP :: a -> (Interval -> a) -> Parser a
pingP manualPing autoPing = fmap (mapMode autoPing (const manualPing)) $ manualModeP (pure ()) <|> autoModeP

versionP :: a -> (Interval -> a) -> Parser a
versionP manualVersion autoVersion = fmap (mapMode autoVersion (const manualVersion)) $ manualModeP (pure ()) <|> autoModeP

randomP :: (Interval -> a) -> Parser a
randomP autoRandom = autoRandom <$> autoIntervalP

type Interval = Int

data Mode manualArgs
    = Auto   Interval
    | Manual manualArgs
    deriving (Show, Eq)

mapMode :: (Interval -> a) -> (manualArgs -> a) -> Mode manualArgs -> a
mapMode fa fm = \case
    Auto i -> fa i
    Manual args -> fm args

autoModeP :: Parser (Mode manualArgs)
autoModeP = Auto <$> autoIntervalP

autoIntervalP :: Parser Interval
autoIntervalP = option auto
    (  long  "auto"
    <> short 'a'
    <> help  "Average client request interval in seconds."
    <> value 30
    <> metavar "SECONDS"
    )

manualModeP :: Parser manualArgs -> Parser (Mode manualArgs)
manualModeP p = flag' Manual fields <*> p
  where fields = short 'm'
              <> long "manual"