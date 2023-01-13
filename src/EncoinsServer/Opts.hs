module EncoinsServer.Opts where

import Control.Monad.Reader      (ask)
import Ledger.Ada                (Ada(..))
import Options.Applicative       (Parser, (<**>), auto, command, execParser, fullDesc, info, help, helper, long, option, progDesc, subparser)
import Options.Applicative.Types (ReadM(..))

--------------------------------- Server ---------------------------------

runWithOpts :: IO ServerMode
runWithOpts = execParser $ info (modeParser <**> helper) fullDesc

data ServerMode = Run | Setup

modeParser :: Parser ServerMode
modeParser = subparser
        (  command "run"   (info (pure Run  ) runDesc)
        <> command "setup" (info (pure Setup) setupDesc)
        )
    where
        runDesc   = progDesc "Default server mode without any preliminary work."
        setupDesc = progDesc "Mint and send ENCOINS beacon token."

--------------------------------- Client ---------------------------------

type LovelaceM = Ada

data EncoinsRequestTerm
    = RPMint LovelaceM
    | RPBurn FilePath
    deriving (Show, Eq)

mintParser :: Parser EncoinsRequestTerm
mintParser = RPMint . Lovelace <$> option auto
    (  long "mint"
    <> help "Token ADA value to mint. Nonnegative integer between 0 and 2^20-1."
    )

burnParser :: Parser EncoinsRequestTerm
burnParser = RPBurn <$> option withoutQuotes
    (  long "burn"
    <> help "Token to burn. A path to minting keys file."
    )

withoutQuotes :: ReadM String
withoutQuotes = ReadM $ ask >>= pure