{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Encoins.Relay.Poll.Main where

import           Cardano.Api                        (writeFileJSON)
import           Control.Monad                      (forM, guard, void, when)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans                (lift)
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Foldable                      (minimumBy)
import           Data.Function                      (on)
import           Data.List                          (partition, sortBy)
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (catMaybes)
import           Data.Ord                           (Down (..))
import           Data.Text                          (Text)
import           Encoins.Relay.Common               (decodeOrErrorFromFile, getResponsesIO, progressBarStyle, withResultSaving)
import           Encoins.Relay.Poll.Config          (PollConfig (..), getConfig)
import           GHC.Generics                       (Generic)
import           Ledger                             (Datum (..), PubKeyHash (..), Slot (..))
import qualified PlutusAppsExtra.Api.Kupo           as Kupo
import           PlutusAppsExtra.Api.Maestro        (MaestroToken, runMaestroInIO)
import           PlutusAppsExtra.IO.ChainIndex.Kupo (getTokenBalanceToSlot)
import qualified PlutusAppsExtra.IO.Maestro         as Maestro
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusAppsExtra.Utils.Maestro      (TxDetailsResponse (..))
import           PlutusLedgerApi.V1                 (Credential (..), FromData (..), StakingCredential (StakingHash))
import qualified PlutusLedgerApi.V2                 as PV2
import           PlutusTx.Builtins                  (decodeUtf8, fromBuiltin)
import           PlutusTx.Builtins.Class            (stringToBuiltinByteString)
import           System.Directory.Extra             (createDirectoryIfMissing)
import           System.ProgressBar                 (Progress (..), incProgress, newProgressBar)

type HasPollEnv = ?pollEnvMaestroToken :: MaestroToken

poll :: Integer -> IO ()
poll pollNo = do
    PollConfig{..} <- getConfig $ "poll" <> show pollNo <> "config.json"
    maestroToken <- decodeOrErrorFromFile pcMaestroTokenFp
    let ?pollEnvMaestroToken = maestroToken
    let folderName = "poll" <> show pollNo <> "files"
    createDirectoryIfMissing True folderName
    !result <- getResult folderName pollNo PollConfig{..}
    let msg = renderResult result
    putStrLn msg
    writeFile (folderName <> "/result.txt") msg
    void $ writeFileJSON (folderName <> "/resultFull.json") result

renderResult :: [(Vote, Integer)] -> String
renderResult votes = mconcat ["Yes: ", renderPercents yes,"\nNo: ", renderPercents no]
    where
        getVotesNum = sum . map snd
        (yesVotes, noVotes) = partition ((== "Yes") . voteText . fst) votes
        totalVotes = getVotesNum votes
        yes = getVotesNum yesVotes
        no = getVotesNum noVotes
        renderPercents p = show @Double ((* 100) (((/) `on` fromIntegral) p totalVotes)) <> "%"

getResult :: HasPollEnv => FilePath -> Integer -> PollConfig -> IO [(Vote, Integer)]
getResult folderName pollNo PollConfig{..} = do
        !responses <- getResponsesIO pcNetworkId pcStart pcFinish 3600
        pb <- newProgressBar (progressBarStyle "Getting votes") 10 (Progress 0 (length responses) ())
        !votes <- fmap catMaybes $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            vote <- withResultSaving (mconcat [folderName, "/vote_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getVoteFromResponse KupoResponse{..}
            lift $ incProgress pb 1
            pure vote

        let votes' = removeDuplicates votes
        pb' <- newProgressBar (progressBarStyle "Calculating each vote weight") 10 (Progress 0 (length votes') ())
        forM votes' $ \Vote{..} -> fmap (Vote{..},) $ do
            balance <- withResultSaving (mconcat [folderName, "/weight_", show voteTxId, "@", show voteTxIdx, ".json"]) $
                getTokenBalanceToSlot pcCs pcTokenName (Just pcFinish) (StakingHash $ PubKeyCredential votePkh)
            incProgress pb' 1
            pure balance
    where
        getVoteFromResponse :: HasPollEnv => KupoResponse -> MaybeT IO Vote
        getVoteFromResponse KupoResponse{..} = do
            dh             <- MaybeT $ pure krDatumHash
            dat            <- MaybeT $ Kupo.getDatumByHash dh
            (pkhBbs, vote) <- MaybeT . pure $ extractVoteFromDatum dat
            pkh            <- MaybeT . pure $ getStakeKey krAddress
            -- Check that tx signed by key specified in datum
            when pcCheckDatumPkh $ do
                response <- MaybeT $ runMaestroInIO pcNetworkId ?pollEnvMaestroToken $ Maestro.getTxDetails krTxId
                let sigs = tdrAdditionalSigners response
                guard $ PubKeyHash pkhBbs `elem` sigs
            let res = Vote pkh (swhhSlot krCreatedAt) krTxId krOutputIndex (fromBuiltin $ decodeUtf8 vote)
            liftIO $ print res
            pure res

        removeDuplicates :: [Vote] -> [Vote]
        removeDuplicates = fmap (minimumBy (compare `on` Down . voteSlot)) . NonEmpty.groupBy ((==) `on` votePkh) . sortBy (compare `on` votePkh)

        extractVoteFromDatum (Datum dat) = case fromBuiltinData dat of
            Just ["ENCOINS", bs1, bs2, bs3] ->
                if bs3 == "Yes" || bs3 == "No" && bs1 == "Poll #" <> stringToBuiltinByteString (show pollNo)
                then Just (bs2, bs3)
                else Nothing
            _ -> Nothing

data Vote = Vote
    { votePkh   :: PubKeyHash
    , voteSlot  :: Slot
    , voteTxId  :: PV2.TxId
    , voteTxIdx :: Integer
    , voteText  :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
