{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -O2 -j16         #-}

module Encoins.Relay.Poll where

import           Cardano.Api                          (FromJSON, ToJSON, writeFileJSON, EraInMode (..))
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Control.Arrow                        ((>>>))
import           Control.Exception                    (try, SomeException, handle)
import           Control.Monad                        (join, (>=>), void, forM)
import           Control.Monad.Extra                  (ifM)
import           Data.Either.Extra                    (eitherToMaybe)
import           Data.Function                        (on, (&))
import           Data.Functor                         ((<&>))
import           Data.List                            (nub, partition)
import           Data.Maybe                           (listToMaybe, mapMaybe, catMaybes)
import           Data.Traversable                     (for)
import           GHC.Generics                         (Generic)
import           Ledger                               (PubKeyHash (..), TxId (..), _decoratedTxOutAddress, Address (..),
                                                       Datum (..), TxOutRef (..), SomeCardanoApiTx (..), PaymentPubKeyHash (..), 
                                                       TokenName, CurrencySymbol, Slot, NetworkId)
import           Ledger.Tx.CardanoAPI                 (fromCardanoTxId, getRequiredSigners)
import           Plutus.V1.Ledger.Api                 (StakingCredential (..), Credential (PubKeyCredential), FromData (..), 
                                                       BuiltinByteString)
import           PlutusAppsExtra.IO.ChainIndex.Kupo   (getKupoResponseByStakeKeyBetweenSlots, getDatumByHashSafe)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo   as Kupo
import           PlutusAppsExtra.IO.ChainIndex.Plutus (getTxFromId)
import qualified PlutusAppsExtra.IO.Blockfrost        as BF
import qualified PlutusAppsExtra.Utils.Blockfrost     as BF
import           PlutusAppsExtra.Utils.Kupo           (KupoResponse(..))
import           System.Directory.Extra               (createDirectoryIfMissing, doesFileExist)

poll1 :: IO ()
poll1 = do
        createDirectoryIfMissing True "pollFiles"
        c <- decodeOrErrorFromFile "pollConfig.json"
        let ?pollConfig = c

        !txOutRefs <- tryReadFile "pollFiles/txOutRefs.json" >>= either (const getTxOutRefs) pure
        writeStageFile 1 txOutRefs

        !pkhs <- tryReadFile "pollFiles/PubKeyHashes.json" >>= either (const $ getVotersPkhs txOutRefs) pure
        writeStageFile 2 pkhs

        !votes <- getVotes pkhs
        writeStageFile 3 votes

        putStrLn "\n\n\n"
        print votes
        let (y, n) = partition (\(_, v, _) -> v == "Yes") votes
            getPercents v = show @Double (((/) `on` (fromIntegral . length)) v votes) <> "%"
        putStrLn $ "Yes: " <> getPercents y
        putStrLn $ "No: " <> getPercents n
    where
        tryReadFile :: FromJSON a => FilePath -> IO (Either SomeException a)
        tryReadFile = try . decodeOrErrorFromFile
        writeStageFile :: ToJSON a => Int -> a -> IO ()
        writeStageFile i s = void $ writeFileJSON ("pollFiles/" <> show i <> ".json") s

data PollConfig = PollConfig
    { pcCurrencySymbol :: CurrencySymbol
    , pcTokenName      :: TokenName
    , pcStart          :: Slot
    , pcFinish         :: Slot
    , pcNetworkId      :: NetworkId
    } deriving (Show, Generic, ToJSON, FromJSON)

type HasPollConfig = (?pollConfig :: PollConfig)

type PollStakeKeyBbs = BuiltinByteString
type PollVoteBbs     = BuiltinByteString
type PollRules = Datum -> Maybe (PollStakeKeyBbs, PollVoteBbs)

poll1Rules :: Datum -> Maybe (BuiltinByteString, BuiltinByteString)
poll1Rules (Datum dat) =  case fromBuiltinData dat of
    Just ["ENCOINS", "Poll #1", bs2, bs3]
        -> if bs3 == "Yes" || bs3 == "No"
           then Just (bs2, bs3)
           else Nothing
    _ -> Nothing

getTxOutRefs :: HasPollConfig => IO [TxOutRef]
getTxOutRefs = reloadHandler getTxOutRefs $ do
        putStrLn "getting asset txOutRefs..."
        map (\BF.AssetTxsResponse{..} -> TxOutRef (fromCardanoTxId atrTxHash) atrTxIndex)
            <$> BF.getAssetTxs (pcNetworkId ?pollConfig) (pcCurrencySymbol ?pollConfig) (pcTokenName ?pollConfig)

getVotersPkhs :: [TxOutRef] -> IO [PubKeyHash]
getVotersPkhs refs = reloadHandler (getVotersPkhs refs) $ do
        putStrLn "geting voters pubkey hashes..."
        fmap (nub . mapMaybe getStakeKey . catMaybes) $ forM (zip [1..] refs) $ \(i, r) -> do
            putStrLn $ show i <> "/" <> show (length refs)
            Kupo.getTxOutFromRef r
    where
        getStakeKey =  _decoratedTxOutAddress >>> \case
            (Address _ (Just (StakingHash (PubKeyCredential pkh)))) -> Just pkh
            _ -> Nothing


getVotes :: HasPollConfig => [PubKeyHash] -> IO [(PollStakeKeyBbs, PollVoteBbs, TxId)]
getVotes pkhs = reloadHandler (getVotes pkhs) $ do
    putStrLn "getting votes..."
    !votes <- go pkhs 0 (show $ length pkhs)
    pure votes
    where
        go :: [PubKeyHash] -> Integer -> String -> IO [(PollStakeKeyBbs, PollVoteBbs, TxId)]
        go []         _ l = putStrLn (l <> "/" <> l) >> pure []
        go (pkh:rest) i l = reloadHandler (go (pkh:rest) i l) $ do
            putStrLn $ "getting votes from " <> show pkh
            putStrLn $ show i <> "/" <> l
            !res <- do
                let fName = "pollFiles/" <> show pkh <> ".json"
                ex <- doesFileExist fName
                if ex
                then try @SomeException (decodeOrErrorFromFile fName) >>= maybe (getVote pkh) pure . eitherToMaybe
                else getVote pkh
            maybe id (\x -> ((x:) <$>)) (join $ listToMaybe res) $ go rest (i + 1) l


        getVote :: HasPollConfig => PubKeyHash -> IO [Maybe (BuiltinByteString, BuiltinByteString, TxId)]
        getVote pkh = do
            !resps <- concat <$> getVoteSlightly (pcStart ?pollConfig) (pcFinish ?pollConfig) pkh
            print $  show pkh <> " has " <> show (length resps) <> "txs"
            for resps $ extractPollRes >=> \case
                Just v -> ifM (isSignedBySameStakeKey v) (pure $ Just v) (pure Nothing)
                _      -> pure Nothing
        extractPollRes KupoResponse{..} = do
            mbDatum <- join <$> sequence (getDatumByHashSafe <$> krDatumHash)
            pure $ (mbDatum >>= poll1Rules) & fmap (\(sk, v) -> (sk, v, krTransactionId))
        getVoteSlightly from to pkh = do
            let xs = [from, from + 36000 .. to]
                intervals = zip (Just <$> init xs) (Just <$> tail xs)
            forM (zip [1 :: Int ..] intervals) $ \(i, (f, t)) -> do
                let fName = "pollFiles/" <> "kupo_response_" <> show pkh <> "_#" <> show i <> ".json"
                ex <- doesFileExist fName
                putStrLn $ "\t" <> show pkh <> "\t" <> show i <> "/" <> show (length intervals)
                !resp <- if ex
                         then try @SomeException (decodeOrErrorFromFile fName) >>= 
                            either (const $ getKupoResponseByStakeKeyBetweenSlots f t pkh) pure
                         else getKupoResponseByStakeKeyBetweenSlots f t pkh
                writeFileJSON fName resp
                pure resp
        isSignedBySameStakeKey :: (PollStakeKeyBbs, PollVoteBbs, TxId) -> IO Bool
        isSignedBySameStakeKey (sk, _, txId) = getTxFromId txId <&> \case
            Just (SomeTx tx BabbageEraInCardanoMode) -> PaymentPubKeyHash (PubKeyHash sk) `elem` getRequiredSigners tx
            _ -> False

reloadHandler :: IO a -> IO a -> IO a
reloadHandler ma = handle (\(e :: SomeException) -> putStrLn (show e <> "\t(Handled)") >> ma)