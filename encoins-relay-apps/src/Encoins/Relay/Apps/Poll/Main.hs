{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Encoins.Relay.Apps.Poll.Main where

import           Cardano.Api                        (writeFileJSON)
import           Control.Monad                      (forM, guard, void, when)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans                (lift)
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Function                      (on)
import           Data.List                          (partition, sortBy)
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (catMaybes)
import           Data.Ord                           (Down (..))
import           Data.Text                          (Text)
import           Encoins.Relay.Apps.Internal        (getResponsesIO, progressBarStyle, withResultSaving)
import           Encoins.Relay.Apps.Poll.Config     (PollConfig (..), getConfig)
import           GHC.Generics                       (Generic)
import           Ledger                             (Datum (..), PubKeyHash (..), Slot (..), TxId (..))
import           Plutus.V1.Ledger.Api               (Credential (..), FromData (..), StakingCredential (StakingHash))
import           PlutusAppsExtra.IO.ChainIndex.Kupo (getTokenBalanceToSlot)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusAppsExtra.Utils.Tx           (txIsSignedByKey)
import           PlutusTx.Builtins                  (decodeUtf8, fromBuiltin)
import           PlutusTx.Builtins.Class            (stringToBuiltinByteString)
import           System.Directory.Extra             (createDirectoryIfMissing)
import           System.ProgressBar                 (Progress (..), incProgress, newProgressBar)

poll :: Integer -> IO ()
poll pollNo = do
    PollConfig{..} <- getConfig $ "poll" <> show pollNo <> "config.json"
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

getResult :: FilePath -> Integer -> PollConfig -> IO [(Vote, Integer)]
getResult folderName pollNo PollConfig{..} = do
        !responses <- getResponsesIO pcNetworkId pcStart pcFinish 3600
        pb <- newProgressBar (progressBarStyle "Getting votes") 10 (Progress 0 (length responses) ())
        !votes <- fmap catMaybes $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            lift $ incProgress pb 1
            withResultSaving (mconcat [folderName, "/vote_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getVoteFromResponse KupoResponse{..}

        let votes' = removeDuplicates votes
        pb' <- newProgressBar (progressBarStyle "Calculating each vote weight") 10 (Progress 0 (length votes') ())
        forM votes' $ \Vote{..} -> fmap (Vote{..},) $ do
            incProgress pb' 1
            withResultSaving (mconcat [folderName, "/weight_", show voteTxId, "@", show voteTxIdx, ".json"]) $
                getTokenBalanceToSlot pcCs pcTokenName (Just pcFinish) (StakingHash $ PubKeyCredential votePkh)
    where
        getVoteFromResponse :: KupoResponse -> MaybeT IO Vote
        getVoteFromResponse KupoResponse{..} = do
            dh             <- MaybeT $ pure krDatumHash
            dat            <- MaybeT $ Kupo.getDatumByHash dh
            (pkhBbs, vote) <- MaybeT . pure $ extractVoteFromDatum dat
            pkh            <- MaybeT . pure $ getStakeKey krAddress
            -- Check that tx signed by key specified in datum
            when pcCheckDatumPkh $ guard =<< MaybeT (Just <$> txIsSignedByKey krTxId pkhBbs)
            let res = Vote pkh (swhhSlot krCreatedAt) krTxId krOutputIndex (fromBuiltin $ decodeUtf8 vote)
            liftIO $ print res
            pure res

        removeDuplicates :: [Vote] -> [Vote]
        removeDuplicates = fmap (NonEmpty.head . NonEmpty.sortBy (compare `on` Down . voteSlot)) . NonEmpty.groupBy ((==) `on` votePkh) . sortBy (compare `on` votePkh)

        extractVoteFromDatum (Datum dat) = case fromBuiltinData dat of
            Just ["ENCOINS", bs1, bs2, bs3] ->
                if bs3 == "Yes" || bs3 == "No" && bs1 == "Poll #" <> stringToBuiltinByteString (show pollNo)
                then Just (bs2, bs3)
                else Nothing
            _ -> Nothing

data Vote = Vote
    { votePkh   :: PubKeyHash
    , voteSlot  :: Slot
    , voteTxId  :: TxId
    , voteTxIdx :: Integer
    , voteText  :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
