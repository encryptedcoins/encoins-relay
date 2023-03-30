{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}

module EncoinsServer.Opts where

import Control.Monad.Reader       (ask)
import Data.Aeson                 (ToJSON)
import Data.Bool                  (bool)
import GHC.Generics               (Generic)
import Ledger.Ada                 (Ada(..))
import Options.Applicative        (Parser, (<**>), auto, command, execParser, fullDesc, info, help, helper, long, option, progDesc, subparser, metavar)
import Options.Applicative.Types  (ReadM(..))
import System.Random              (Random(..))

import ENCOINS.Bulletproofs.Types (Secret)

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
    | RPBurn (Either Secret FilePath)
    deriving (Show, Eq, Generic, ToJSON)

instance Random EncoinsRequestTerm where
    random g = 
        let (b, g')   = random g
            (a, g'')  = random g'
            (s, g''') = random g''
        in bool (RPMint $ Lovelace (abs a `mod` 10_000_000_000), g'') (RPBurn $ Left s, g''') b
    randomR _ = random

instance Random [EncoinsRequestTerm] where
    random g  =
        let (reqTerms, gNew) = foldr (\_ (lst, g') -> let (e, g'') = random g' in (e:lst, g'')) ([], g) [1 :: Integer .. 5]
        in (reqTerms, gNew)
    randomR _ = random

mintParser :: Parser EncoinsRequestTerm
mintParser = RPMint . Lovelace <$> option auto
    (  long "mint"
    <> help "Token ADA value to mint. Nonnegative integer between 0 and 2^20-1."
    <> metavar "ADA"
    )

burnParser :: Parser EncoinsRequestTerm
burnParser = RPBurn . Right <$> option withoutQuotes
    (  long "burn"
    <> help "Token to burn. A path to minting keys file."
    <> metavar "FILEPATH"
    )

withoutQuotes :: ReadM String
withoutQuotes = ReadM $ ask >>= pure