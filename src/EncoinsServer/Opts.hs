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
        setupDesc = progDesc "Mint and send encoins beacon token."

--------------------------------- Client ---------------------------------

type LovelaceM = Ada

data EncoinsRequestPiece
    = RPMint LovelaceM
    | RPBurn FilePath
    deriving (Show, Eq)

mintParser :: Parser EncoinsRequestPiece
mintParser = RPMint . Lovelace <$> option auto
    (  long "mint"
    <> help "Token name to mint."
    )

burnParser :: Parser EncoinsRequestPiece
burnParser = RPBurn <$> option withoutQuotes
    (  long "burn"
    <> help "Token name to burn."
    )

withoutQuotes :: ReadM String
withoutQuotes = ReadM $ ask >>= pure