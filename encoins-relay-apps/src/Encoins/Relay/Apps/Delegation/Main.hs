{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Encoins.Relay.Apps.Delegation.Main where

import           Cardano.Api                           (writeFileJSON)
import           Cardano.Server.Config                 (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Utils.Wait             (waitTime)
import           Control.Applicative                   ((<|>))
import           Control.Arrow                         (Arrow ((&&&)))
import           Control.Concurrent.Async              (async, wait)
import           Control.Monad                         (MonadPlus (mzero), forM, forever, guard, void, when)
import           Control.Monad.Trans.Maybe             (MaybeT (..))
import           Data.Aeson                            (FromJSON, ToJSON)
import           Data.Aeson.Types                      (parseEither)
import           Data.Function                         ((&))
import           Data.Functor                          ((<&>))
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, listToMaybe, mapMaybe)
import           Data.Text                             (Text)
import qualified Data.Time                             as Time
import qualified Encoins.Relay.Apps.Delegation.V1.Main as V1
import           Encoins.Relay.Apps.Internal           (formatTime, loadMostRecentFile, newProgressBar)
import           Encoins.Relay.Server.Config           (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Delegation       (Delegation (..), removeDuplicates)
import           GHC.Generics                          (Generic)
import           Ledger                                (Address (..), Datum (..), DatumFromQuery (..), NetworkId, PubKeyHash (..),
                                                        TxId, TxOutRef (..))
import           Plutus.V1.Ledger.Api                  (CurrencySymbol, FromData (..), TokenName, fromBuiltin)
import qualified PlutusAppsExtra.IO.Blockfrost         as Bf
import qualified PlutusAppsExtra.IO.Maestro            as Maestro
import           PlutusAppsExtra.Utils.Address         (getStakeKey)
import           PlutusAppsExtra.Utils.Maestro         (TxDetailsOutput (..), TxDetailsResponse (..))
import           PlutusAppsExtra.Utils.Time            (parseSlotConfig)
import           PlutusTx.Builtins                     (decodeUtf8)
import           System.Directory                      (createDirectoryIfMissing)
import           System.ProgressBar                    (incProgress)

type HasDelegEnv =
    ( ?networkId :: NetworkId
    , ?cs        :: CurrencySymbol
    , ?tokenName :: TokenName
    , ?checkSig  :: Bool -- There is no signature checks in tests untill cardano-wallet signature fix
    )                    -- https://github.com/cardano-foundation/cardano-wallet/issues/4104
                         -- (They are still present outside of tests)

main :: FilePath -> IO ()
main configFp = do
        config      <- decodeOrErrorFromFile configFp
        relayConfig <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
        slotConfig  <- either error id . parseEither parseSlotConfig <$> decodeOrErrorFromFile (cSlotConfigFile config)
        let delgFolder = cDelegationFolder relayConfig
        createDirectoryIfMissing True delgFolder
        let ?slotConfig       = slotConfig
            ?networkId        = cNetworkId config
            ?cs               = cDelegationCurrencySymbol relayConfig
            ?tokenName        = cDelegationTokenName relayConfig
            ?checkSig         = True
        forever $ do
            ct           <- Time.addUTCTime (- 300) <$> Time.getCurrentTime
            delay        <- async $ waitTime 300
            pastProgress <- loadProgressFile delgFolder
                        <|> loadV1ProgressFile delgFolder
                        <|> initProgress
            newProgress  <- updateProgress pastProgress
            void $ writeFileJSON (delgFolder <> "/delegatorsV2_" <> formatTime ct <> ".json") newProgress
            ipsWithBalances <- getIpsWithBalances $ pDelgations newProgress
            V1.writeResultFile delgFolder ct ipsWithBalances
            wait delay
    where
        loadProgressFile delegFolder = loadMostRecentFile delegFolder "delegatorsV2_" >>=
            maybe mzero (pure . snd)
        loadV1ProgressFile delegFolder = loadMostRecentFile delegFolder "delegators_" >>=
            maybe mzero (pure . uncurry Progress . ((listToMaybe . fmap (txOutRefId . delegTxOutRef)) &&& id) . snd)
        initProgress = do
            putStrLn "No past delegations found."
            pure $ Progress Nothing []

getIpsWithBalances :: HasDelegEnv => [Delegation] -> IO (Map Text Integer)
getIpsWithBalances delegs = V1.concatIpsWithBalances <$> do
    balances <- Maestro.getAccountAddressesHoldingAssets ?networkId ?cs ?tokenName
    pure $ delegs & mapMaybe (\Delegation{..} -> Map.lookup delegStakeKey balances <&> (delegIp,))

data Progress = Progress
    { pLastTxId   :: Maybe TxId
    , pDelgations :: [Delegation]
    } deriving (Show, Generic, FromJSON, ToJSON)

updateProgress :: HasDelegEnv => Progress -> IO Progress
updateProgress Progress{..} = do
    txIds     <- Bf.getAllAssetTxsAfterTxId ?networkId ?cs ?tokenName pLastTxId
    pb        <- newProgressBar "Getting delegations" (length txIds)
    newDelegs <- catMaybes <$> mapM (const (incProgress pb 1) >> findDeleg) txIds
    putStrLn "New delegs:"
    print newDelegs
    pure $ Progress (listToMaybe txIds) $ removeDuplicates $ pDelgations <> newDelegs

findDeleg :: HasDelegEnv => TxId -> IO (Maybe Delegation)
findDeleg txId = runMaybeT $ do
    TxDetailsResponse{..} <- MaybeT $ Maestro.getTxDetails ?networkId txId
    MaybeT $ fmap (listToMaybe . catMaybes) $ forM tdrOutputs $ \TxDetailsOutput{..} -> runMaybeT $ do
        stakeKey  <- hoistMaybe $ getStakeKey tdoAddress
        (dh, dfq) <- hoistMaybe tdoDatum
        Datum dat <- case dfq of
            DatumUnknown   -> MaybeT $ Bf.getDatumByHash ?networkId dh
            DatumInline da -> pure da
            DatumInBody da -> pure da
        ["ENCOINS", "Delegate", skBbs, ipBbs] <- hoistMaybe $ fromBuiltinData dat
        let ipAddr = fromBuiltin $ decodeUtf8 ipBbs
        when ?checkSig $ guard $ PubKeyHash skBbs `elem` tdrAdditionalSigners && V1.isValidIp ipAddr
        pure $ Delegation (addressCredential tdoAddress) stakeKey (TxOutRef tdoTxHash tdoIndex) tdrSlot ipAddr
    where
        hoistMaybe = MaybeT . pure