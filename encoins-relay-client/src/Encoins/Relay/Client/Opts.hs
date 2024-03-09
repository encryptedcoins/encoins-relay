{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Encoins.Relay.Client.Opts where

import qualified CSL
import           Cardano.Server.Client.Client               (HasServantClientEnv, autoClientWith, autoPing, autoVersion, manualClient,
                                                             manualPing, manualVersion, pingClient, sendRequest, versionClient)
import           Cardano.Server.Client.Gen                  (randomAddressBech32Text)
import           Cardano.Server.Client.Opts                 (Interval, autoModeP, manualModeP, mapMode, pingCommand, randomCommand,
                                                             versionCommand)
import           Cardano.Server.EndpointName                (GetEndpointName)
import           Cardano.Server.Endpoints.Ping              (PingApi)
import           Cardano.Server.Endpoints.Submit            (SubmitTxApi, SubmitTxReqBody (..))
import           Cardano.Server.Endpoints.Utxos             (UtxosApi)
import           Cardano.Server.Endpoints.Version           (VersionApi)
import           Cardano.Server.Error.Servant               (EndpointEnvelope)
import           Cardano.Server.Internal                    (AppT, runAppT)
import           Cardano.Server.Utils.Wait                  (waitTime)
import           Control.Monad                              (forever, guard, void)
import           Control.Monad.Catch                        (throwM, MonadCatch)
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Control.Monad.Reader                       (MonadReader (..))
import           Data.Data                                  (Proxy (..))
import           Data.Functor                               ((<&>))
import           Data.List                                  (isPrefixOf)
import           Data.Text                                  (Text)
import           ENCOINS.Bulletproofs                       (BulletproofSetup)
import           ENCOINS.Core.OffChain                      (EncoinsMode (..))
import           Encoins.Relay.Client.Client                (txClientRedeemer)
import           Encoins.Relay.Client.Secrets               (EncoinsRequestTerm (..), genTerms)
import           Encoins.Relay.Server.Endpoints.Tx.Intenral (InputOfEncoinsApi)
import           Encoins.Relay.Server.Endpoints.Tx.New      (NewTxApi)
import           Encoins.Relay.Server.Endpoints.Tx.Server   (ServerTxApi)
import           Encoins.Relay.Server.Internal              (EncoinsRelayEnv (..))
import           Encoins.Relay.Server.Server                (EncoinsApi)
import           Encoins.Relay.Server.Status                (EncoinsStatusReqBody (..), StatusApi)
import           GHC.TypeLits                               (KnownSymbol)
import           Options.Applicative                        (Alternative ((<|>)), ParseError (ErrorMsg), Parser, abortOption, argument,
                                                             auto, command, hsubparser, info, metavar, progDesc, some, strArgument, value)
import           Options.Applicative.Types                  (readerAsk)
import           PlutusAppsExtra.Utils.Network              (HasNetworkId (getNetworkId))
import           Servant.Client                             (ClientM, HasClient (..), client)
import           System.Random                              (randomIO, randomRIO)
import           Test.QuickCheck                            (Arbitrary (..), generate)

data ClientCommand
    = AutoPing Interval
    | AutoVersion Interval
    | AutoUtxos Interval
    | AutoNewTxRedeemer Interval
    | AutoServerTxRedeemer Interval
    | AutoSumbitTx Interval
    | AutoStatus Interval
    | ManualPing
    | ManualVersion
    | ManualUtxos Text
    | ManualNewTxRedeemer EncoinsMode [EncoinsRequestTerm]
    | ManualServerTxRedeemer EncoinsMode [EncoinsRequestTerm]
    | ManualSumbitTx SubmitTxReqBody
    | ManualStatus EncoinsStatusReqBody
    | RandomEndpoint Interval

runCommand :: (?bulletproofSetup :: BulletproofSetup, HasServantClientEnv, MonadIO m, MonadCatch m) => ClientCommand -> AppT EncoinsApi m ()
runCommand = \case
    AutoPing i -> autoPing i
    AutoVersion i -> autoVersion i
    AutoUtxos i -> autoClientWith @UtxosApi genUtxosReq i utxosClient
    AutoNewTxRedeemer i -> forever $ autoTxAppTClient @NewTxApi >> randomRIO (1, i * 2) >>= waitTime
    AutoServerTxRedeemer i -> forever $ autoTxAppTClient @ServerTxApi >> randomRIO (1, i * 2) >>= waitTime
    AutoSumbitTx i -> autoClientWith @SubmitTxApi (error "AutoSubmitTx isn't implemented yet.") i submitTxClient
    AutoStatus i -> autoClientWith @StatusApi genStatusReqBody i statusClient
    ManualPing -> void manualPing
    ManualVersion -> void manualVersion
    ManualUtxos addr -> void $ manualClient @UtxosApi $ utxosClient addr
    ManualNewTxRedeemer mode input -> let ?mode = mode in void $ fst =<< txClientRedeemer @NewTxApi input
    ManualServerTxRedeemer mode input -> let ?mode = mode in void $ fst =<< txClientRedeemer @ServerTxApi input
    ManualSumbitTx tx -> void $ manualClient @SubmitTxApi $ submitTxClient tx
    ManualStatus reqBody -> void $ manualClient @StatusApi   $ statusClient reqBody
    RandomEndpoint i     -> forever $ do
        liftIO (randomRIO @Int (0, 5)) >>= \case
            0 -> void $ sendRequest @PingApi pingClient
            1 -> void $ sendRequest @VersionApi versionClient
            2 -> void $ sendRequest @UtxosApi . utxosClient =<< genUtxosReq
            3 -> void $ autoTxAppTClient @NewTxApi
            4 -> void $ autoTxAppTClient @ServerTxApi
            5 -> void $ sendRequest @StatusApi . statusClient =<< randomIO
            _ -> void $ error "AutoSubmitTx isn't implemented yet."
        waitTime =<< randomRIO (1, i * 2)

autoTxAppTClient :: forall e m.
    ( ?bulletproofSetup :: BulletproofSetup
    , Client ClientM e ~ ((InputOfEncoinsApi, CSL.TransactionInputs) -> ClientM (EndpointEnvelope e))
    , HasServantClientEnv
    , HasClient ClientM e
    , Show (EndpointEnvelope e)
    , KnownSymbol (GetEndpointName e)
    , MonadIO m
    , MonadCatch m
    ) => AppT EncoinsApi m (EndpointEnvelope e)
autoTxAppTClient = do
    env <- ask
    mode <- genEncoinsMode
    let ?mode = mode
    terms <- genTerms
    sendRequest @e $ liftIO $ runAppT env $ do
        (a, res) <- txClientRedeemer @e terms
        a
        either throwM pure res

utxosClient :: Text -> ClientM (EndpointEnvelope UtxosApi)
utxosClient = client (Proxy @UtxosApi)

newTxClient :: (InputOfEncoinsApi, CSL.TransactionInputs) -> ClientM (EndpointEnvelope NewTxApi)
newTxClient = client (Proxy @NewTxApi)

submitTxClient :: SubmitTxReqBody -> ClientM (EndpointEnvelope SubmitTxApi)
submitTxClient = client (Proxy @SubmitTxApi)

statusClient :: EncoinsStatusReqBody -> ClientM (EndpointEnvelope StatusApi)
statusClient = client (Proxy @StatusApi)

commandParser :: Parser ClientCommand
commandParser = hsubparser $ mconcat

    [ pingCommand ManualPing AutoPing

    , versionCommand ManualVersion AutoVersion

    , randomCommand RandomEndpoint

    , command "utxos" $ info utxosP
        $ progDesc "Client for utxos endpoint."

    , command "newTx" $ info (txP AutoNewTxRedeemer ManualNewTxRedeemer)
        $ progDesc "Client for encoins tx in newTx endpoint."

    , command "serverTx" $ info (txP AutoServerTxRedeemer ManualServerTxRedeemer)
        $ progDesc "Client for encoins tx in serverTx endpoint."

    , command "submitTx" $ info submitTxP
        $ progDesc "Client for submitTx endpoint."

    , command "status" $ info statusP
        $ progDesc "Client for status endpoint."
    ]

utxosP :: Parser ClientCommand
utxosP = fmap (mapMode AutoUtxos ManualUtxos) $ manualModeP (strArgument addrFields) <|> autoModeP
  where
    addrFields = mconcat []

txP :: (Interval -> ClientCommand) -> (EncoinsMode -> [EncoinsRequestTerm] -> ClientCommand) -> Parser ClientCommand
txP autoCommand manualCommand = fmap (mapMode autoCommand (uncurry manualCommand))
    $ manualModeP ((,) <$> encoinsModeP <*> some requestTermP)
    <|> autoModeP

submitTxP :: Parser ClientCommand
submitTxP = fmap (mapMode AutoSumbitTx ManualSumbitTx)
    $ manualModeP (SubmitTxReqBody <$> strArgument mempty <*> argument auto mempty)
    <|> (abortOption (ErrorMsg "AutoSubmitTx isn't implemented yet.") mempty <*> autoModeP)

statusP :: Parser ClientCommand
statusP = fmap (mapMode AutoStatus ManualStatus)
    $ manualModeP (maxAdaWithdrawP <|> ledgerEncoinsP) <|> autoModeP
  where
    maxAdaWithdrawP = argument auto maxAdaWithdrawModes
    maxAdaWithdrawModes = mconcat []
    ledgerEncoinsP  = argument auto ledgerEncoinsModes
    ledgerEncoinsModes = mconcat []

encoinsModeP :: Parser EncoinsMode
encoinsModeP = argument auto
    (  value WalletMode
    <> metavar "WalletMode | LedgerMode"
    )

requestTermP :: Parser EncoinsRequestTerm
requestTermP = mintTermP <|> burnTermP
  where
    mintTermP = RPMint . fromInteger <$> argument auto mintModes
    mintModes = mconcat []
    burnTermP = fmap (RPBurn . Right . ("secrets/" <>) . (<> ".json")) $ flip argument burnModes
        $ readerAsk >>= \str -> str <$ guard ("0x" `isPrefixOf` str)
    burnModes = mconcat []

genStatusReqBody :: MonadIO m => m EncoinsStatusReqBody
genStatusReqBody = randomIO <&> \b -> if b then MaxAdaWithdraw else LedgerEncoins

genUtxosReq :: MonadIO m => AppT EncoinsApi m Text
genUtxosReq = do
    networkId <- getNetworkId
    liftIO $ randomRIO @Int (0, 20) >>= \case
        0 -> generate arbitrary
        _ -> randomAddressBech32Text networkId

genEncoinsMode :: MonadIO m => m EncoinsMode
genEncoinsMode = randomIO <&> \b -> if b then WalletMode else LedgerMode

-- data Options = Options
--     { optsEndpoint    :: ServerEndpoint
--     , optsMode        :: Mode
--     , optsEncoinsMode :: EncoinsMode
--     } deriving (Show, Eq)

-- runWithOpts :: IO Options
-- runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

-- optionsParser :: Parser Options
-- optionsParser = liftA3 Options
--     serverEndpointParser
--     (autoModeParser <|> manualModeParser)
--     encoinsModeParser

-- encoinsModeParser :: Parser EncoinsMode
-- encoinsModeParser = argument auto
--     (  value WalletMode
--     <> metavar "WalletMode | LedgerMode"
--     )

-- extractCommonOptions :: Options -> CommonOptions
-- extractCommonOptions Options{..} = CommonOptions optsEndpoint optsMode

-- data EncoinsRequestTerm
--     = RPMint Ada
--     | RPBurn (Either Secret FilePath)
--     deriving (Show, Eq, Generic, ToJSON)

-- instance Random EncoinsRequestTerm where
--     random g =
--         let (b, g')   = random g
--             (a, g'')  = random g'
--             (s, g''') = random g''
--         in bool (RPMint $ Lovelace a, g'') (RPBurn $ Left s, g''') b
--     randomR _ = random

-- instance Random [EncoinsRequestTerm] where
--     random g  =
--         let (reqTerms, gNew) = foldr (\_ (lst, g') -> let (e, g'') = random g' in (e:lst, g'')) ([], g) [1 :: Integer .. 5]
--         in (reqTerms, gNew)
--     randomR _ = random

-- readRequestTerms :: Text -> Maybe [EncoinsRequestTerm]
-- readRequestTerms = mapM (readTerm . T.unpack) . T.splitOn ","
--     where
--         readTerm = \case
--             'b':t   -> Just $ RPBurn $ Right $ "secrets/" <> t <> ".json"
--             'm':ada -> fmap (RPMint . fromInteger) . readMaybe $ ada
--             _       -> Nothing

-- readAddressValue :: Text -> Maybe (Address, CSL.Value)
-- readAddressValue (T.splitOn "," -> addr:rest) = (,) <$> bech32ToAddress addr <*> readMaybe (T.unpack $ T.intercalate "," rest)
-- readAddressValue _ = Nothing

-- readAddressIpAddress :: Text -> Maybe (Address, Text)
-- readAddressIpAddress (T.splitOn "," -> addr:rest) = (,) <$> bech32ToAddress addr <*> pure (T.intercalate "," rest)
-- readAddressIpAddress _ = Nothing