{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Client.OptsSum where

import Client.Opts         (optionsParser, Options)
import Client.Internal     (HasClient)
import EncoinsServer.Main  (EncoinsServer)
import TestingServer.Main  (TestingServer)
import Options.Applicative (Parser, (<**>), (<|>), fullDesc, info, long, strOption, execParser, helper)

data OptionsSum 
    = OptionsEncoins (Options EncoinsServer)
    | OptionsTesting (Options TestingServer)

runWithOptsSum :: IO OptionsSum
runWithOptsSum = execParser $ info (parserSum <**> helper) fullDesc
    where
        parserSum = parser "encoins" OptionsEncoins
                <|> parser "testing" OptionsTesting

        parser :: forall s. HasClient s => String -> (Options s -> OptionsSum) -> Parser OptionsSum
        parser name constr = constr <$> (strOption @String (long name) *> optionsParser)