module Encoins.Opts where

import Control.Monad.Reader      (ask)
import Ledger.Ada                (Ada(..))
import Options.Applicative       (Parser, auto, help, long, option)
import Options.Applicative.Types (ReadM(..))

data EncoinsRequestPiece
    = RPMint Ada
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