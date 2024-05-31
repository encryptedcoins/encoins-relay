{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Encoins.Relay.Client.Opts where

import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Functor                           ((<&>))
import           Data.List                              (isPrefixOf)
import           Data.Text                              (Text)
import           ENCOINS.Core.OffChain                  (EncoinsMode (..))
import           Encoins.Relay.Client.Secrets           (EncoinsRequestTerm (..))
import           Encoins.Relay.Common.Client.Gen        (randomAddressBech32Text)
import           Encoins.Relay.Common.Client.Opts       (Interval, autoModeP, manualModeP, mapMode, pingCommand, randomCommand,
                                                         versionCommand)
import           Encoins.Relay.Server.Endpoint.Status   (EncoinsStatusReqBody (..))
import           Encoins.Relay.Server.Endpoint.SubmitTx (SubmitTxReqBody (..))
import           Encoins.Relay.Server.Internal          (EncoinsServerM)
import           Options.Applicative                    (Alternative ((<|>)), ParseError (ErrorMsg), Parser, abortOption, argument, auto,
                                                         command, help, hsubparser, info, maybeReader, metavar, progDesc, some, strArgument,
                                                         value)
import           PlutusAppsExtra.Utils.Network          (HasNetworkId (getNetworkId))
import           System.Random                          (randomIO, randomRIO)
import           Test.QuickCheck                        (Arbitrary (..), generate)
import           Text.Read                              (readMaybe)

-- Command

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
    deriving (Show)

-- Parsers

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
    addrFields = mconcat
        [ metavar "address"
        , help "Wallet address."
        ]

txP :: (Interval -> ClientCommand) -> (EncoinsMode -> [EncoinsRequestTerm] -> ClientCommand) -> Parser ClientCommand
txP autoCommand manualCommand = fmap (mapMode autoCommand (uncurry manualCommand))
    $ manualModeP ((,) <$> encoinsModeP <*> requestTermsP)
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
    ledgerEncoinsP = argument auto ledgerEncoinsModes
    ledgerEncoinsModes = mconcat []

encoinsModeP :: Parser EncoinsMode
encoinsModeP = argument auto $ mconcat
    [ value WalletMode
    , metavar "WalletMode | LedgerMode"
    ]

requestTermsP :: Parser [EncoinsRequestTerm]
requestTermsP = some $ flip argument modes $ fmap (either mkMintTerm mkBurnTerm) $ maybeReader $ \str -> if
        | "0x" `isPrefixOf` str         -> Just $ Right str
        | Just mintVal <- readMaybe str -> Just $ Left mintVal
        | otherwise                     -> Nothing
  where
    mkMintTerm = RPMint . fromInteger
    mkBurnTerm = RPBurn . Right . ("secrets/" <>) . (<> ".json")
    modes = mconcat
        [ metavar "₳ | token name"
        , help "Ada value of this token or token name in 0x0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef format."
        ]

genStatusReqBody :: MonadIO m => m EncoinsStatusReqBody
genStatusReqBody = randomIO <&> \b -> if b then MaxAdaWithdraw else LedgerEncoins

genUtxosReq :: EncoinsServerM Text
genUtxosReq = do
    networkId <- getNetworkId
    liftIO $ randomRIO @Int (0, 20) >>= \case
        0 -> generate arbitrary
        _ -> randomAddressBech32Text networkId

genEncoinsMode :: MonadIO m => m EncoinsMode
genEncoinsMode = randomIO <&> \b -> if b then WalletMode else LedgerMode
