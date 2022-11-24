{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Client.Main where

import           Client.Internal           (HasClient(..), ClientM, ClientRequestOf, runClientM)
import           Client.Opts               (AutoOptions(..), Maximum, Options(..), optionsParser)
import           Control.Monad.Reader      (MonadIO(..), forever, when)
import           Data.Aeson                (encode)
import           Data.List                 (nub)
import qualified Data.Text                 as T
import           EncoinsServer.Main        (EncoinsServer)
import           Network.HTTP.Client       (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                            Manager, Request(..), RequestBody(..), responseStatus)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (status204)
import           Options.Applicative       (Parser, (<**>), (<|>), fullDesc, info, long, execParser, helper, flag')
import qualified Server.Internal           as Server
import           System.Random             (randomRIO)
import           TestingServer.Main        (TestingServer)
import           Utils.Logger              (HasLogger(..), (.<))
import           Utils.Wait                (waitTime)
import Control.Monad (replicateM)

main :: IO ()
main = runWithOptsSum >>= \case
        OptionsEncoins opts -> startClient opts
        OptionsTesting opts -> startClient opts
    where
        runWithOptsSum = execParser $ info (parserSum <**> helper) fullDesc
        parserSum = parser "encoins" OptionsEncoins
                <|> parser "test"    OptionsTesting

        parser :: forall s. HasClient s => String -> (Options s -> OptionsSum) -> Parser OptionsSum
        parser name constr = flag' constr (long name) <*> optionsParser

data OptionsSum 
    = OptionsEncoins (Options EncoinsServer)
    | OptionsTesting (Options TestingServer)

startClient :: forall s. HasClient s => Options s -> IO ()
startClient opts = do
    Server.Config{..} <- Server.loadConfig @s
    let fullAddress = "http://"
                   <> T.unpack confServerAddress
                   <> "/relayRequestMint"
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    let mkRequest' = mkRequest @s nakedRequest manager
    runClientM confAuxiliaryEnv confWallet $ withGreetings $ case opts of
        Manual cReq          -> mkRequest' cReq
        Auto AutoOptions{..} -> forever $ do
            cReq <- genRequest @s maxTokensInReq
            mkRequest' cReq
            waitTime =<< randomRIO (1, averageRequestInterval * 2)
    where
        withGreetings = (logMsg "Starting client..." >>)

mkRequest :: forall s. HasClient s => Request -> Manager -> ClientRequestOf s -> ClientM s ()
mkRequest nakedReq manager clientReq = do
        logMsg $ "New tokens to send:\n" .< clientReq
        (onSuccess, red) <- mkRedeemer clientReq
        let req = nakedReq
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode red
                , requestHeaders = [(hContentType, "application/json")]
                }
        resp <- liftIO $ httpLbs req manager
        logMsg $ "Received response:" .< resp
        when (successful resp) onSuccess
    where
        successful = (== status204) . responseStatus

genRequest :: forall s m. HasClient s => MonadIO m => Maximum -> m (ClientRequestOf s)
genRequest ub = liftIO $ do
    len <- randomRIO (1, ub)
    nub <$> replicateM len (genRequestPiece @s)