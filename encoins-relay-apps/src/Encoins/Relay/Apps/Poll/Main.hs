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
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (FromJSON, ToJSON, eitherDecodeFileStrict)
import           Data.Default                       (def)
import           Data.Function                      (on)
import           Data.List                          (partition, sortBy)
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (catMaybes)
import           Data.Ord                           (Down (..))
import           Data.Text                          (Text)
import qualified Data.Text.IO                       as T
import           Encoins.Relay.Apps.Internal        (partiallyGet, withResultSaving)
import           Encoins.Relay.Apps.Poll.Config     (PollConfig (..))
import           GHC.Generics                       (Generic)
import           Ledger                             (Datum (..), DatumHash, PubKeyHash (..), Slot (..), TxId (..))
import           Plutus.V1.Ledger.Api               (Credential (..), FromData (..), StakingCredential (StakingHash))
import           PlutusAppsExtra.IO.ChainIndex.Kupo (CreatedOrSpent (..), KupoRequest (..), SpentOrUnspent (..),
                                                     getTokenBalanceToSlot)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusAppsExtra.Utils.Tx           (txIsSignedByKey)
import           PlutusTx.Builtins                  (decodeUtf8, fromBuiltin)
import           PlutusTx.Builtins.Class            (stringToBuiltinByteString)
import           System.Directory.Extra             (createDirectoryIfMissing)

poll :: Integer -> IO ()
poll pollNo = do
    PollConfig{..} <- fmap (either error id) . eitherDecodeFileStrict $ "poll" <> show pollNo <> "config.json"
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
        putStrLn "Getting reponses..."
        !responses <- getResponses

        putStrLn "Getting votes..."
        !votes <- fmap catMaybes $ forM responses $ \KupoResponse{..} -> runMaybeT $ do
            withResultSaving (mconcat [folderName, "/vote_", show krTxId, "@", show krOutputIndex, ".json"]) $
                getVoteFromResponse KupoResponse{..}

        putStrLn "Calculating each vote weight..."
        let l = show (length votes)
        forM (zip [1 :: Int ..] $ removeDuplicates votes) $ \(i, Vote{..}) -> fmap (Vote{..},) $ do
            liftIO $ putStrLn (show i <> "/" <> l)
            withResultSaving (mconcat [folderName, "/weight_", show voteTxId, "@", show voteTxIdx, ".json"]) $
                getTokenBalanceToSlot pcCs pcTokenName (Just pcFinish) (StakingHash $ PubKeyCredential votePkh)

    where
        getResponses :: IO [KupoResponse]
        getResponses =
            let req = def @(KupoRequest 'SUSpent 'CSCreated 'CSCreated)
            in partiallyGet pcNetworkId T.putStrLn pcStart pcFinish 3600 req (folderName <> "/responses_")

        getVoteFromResponse :: KupoResponse -> MaybeT IO Vote
        getVoteFromResponse KupoResponse{..} = do
            dh <- MaybeT $ pure krDatumHash
            dat <- getDatum dh
            (pkhBbs, vote) <- MaybeT . pure $ extractVoteFromDatum dat
            pkh <- MaybeT . pure $ getStakeKey krAddress
            -- Check that tx signed by key specified in datum
            when pcCheckDatumPkh $ guard =<< MaybeT (Just <$> txIsSignedByKey krTxId pkhBbs)
            let res = Vote pkh (swhhSlot krCreatedAt) krTxId krOutputIndex (fromBuiltin $ decodeUtf8 vote)
            liftIO $ print res
            pure res

        getDatum :: DatumHash -> MaybeT IO Datum
        getDatum dh = MaybeT $ Kupo.getDatumByHash dh

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
