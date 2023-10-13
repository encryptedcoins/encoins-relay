{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Config           (decodeOrErrorFromFile)
import           Cardano.Server.Internal         (loadEnv, runServerM)
import           Cardano.Server.Main             (runServer)
import           Encoins.Relay.Server.Delegation (distributeRewards)
import           Encoins.Relay.Server.Opts       (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server     (mkServerHandle, serverSetup)

import qualified Data.Text                       as T
import           Data.Version                    (showVersion)
import           Development.GitRev              (gitCommitDate, gitDirty, gitHash)
import           Paths_encoins_relay_server      (version)
import           Prettyprinter                   (Pretty (pretty), annotate, defaultLayoutOptions, layoutSmart)
import           Prettyprinter.Render.Terminal   (Color (Blue, Green, Red), bold, color, renderStrict)

relayVersion :: String
relayVersion = T.unpack $ T.intercalate "\n" $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
    where
        sVersion = textToColorText green $ "Encoins-relay-server" <> "v" <> T.pack (showVersion version)
        sHash = " ➤ " <> (textToColorText blue ("Git revision: " :: T.Text) <> $(gitHash))
        sDate = " ➤ " <> (textToColorText blue ("Commit date:  " :: T.Text) <> $(gitCommitDate))
        sDirty = textToColorText red ("There are non-committed files." :: T.Text)
        textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt
        red = annotate $ color Red <> bold
        green = annotate $ color Green <> bold
        blue = annotate $ color Blue <> bold

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    putStrLn relayVersion
    config           <- decodeOrErrorFromFile cardanoServerConfigFp
    runWithOpts >>= \case
        Run      -> mkServerHandle config >>= runServer config
        Setup    -> mkServerHandle config >>= loadEnv config >>= (`runServerM` serverSetup)
        Reward r -> mkServerHandle config >>= loadEnv config >>= (`runServerM` distributeRewards config r)