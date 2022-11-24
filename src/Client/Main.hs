{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Client.Main where
    
import           Client.Internal           (HasClient(..), ClientM, ClientRequestOf, runClientM)
import           Client.Opts               (AutoOptions(AutoOptions, maxTokensInReq, averageRequestInterval), Maximum, Options(..))               
import           Client.OptsSum            (runWithOptsSum, OptionsSum(OptionsTesting, OptionsEncoins)) 
import           Control.Monad             (replicateM)
import           Control.Monad.Reader      (MonadIO(..), forever, when)
import           Data.Aeson                (encode)
import           Data.Functor              ((<&>))
import           Data.List                 (nub)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                            Manager, Request(..), RequestBody(..), responseStatus)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (status204)
import qualified Server.Internal           as Server
import           System.Random             (randomRIO)
import           Utils.Logger              (HasLogger(..), (.<))
import           Utils.Wait                (waitTime)

main :: IO ()
main = runWithOptsSum >>= \case
    OptionsEncoins       opts -> startClient opts
    OptionsTesting opts -> startClient opts

startClient :: forall s. HasClient s => Options s -> IO ()
startClient opts = do
    Server.Config{..} <- Server.loadConfig @s
    let fullAddress = "http://"
                   <> T.unpack confServerAddress
                   <> "/relayRequestMint"
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    let mkRequest' = mkRequest @s nakedRequest manager
    runClientM confAuxiliaryEnv confWallet $ logMsg "Starting client..." >> case opts of
        Manual br            -> mkRequest' br
        Auto AutoOptions{..} -> forever $ do
            br <- genRequest @s maxTokensInReq
            mkRequest' br
            waitTime =<< randomRIO (1, averageRequestInterval * 2)

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
genRequest ub = liftIO $ randomRIO (1, ub) >>= flip replicateM (genRequestPiece @s) <&> nub