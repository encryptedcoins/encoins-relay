{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Client.Secrets where

import           Cardano.Server.Internal       (AppT)
import           Control.Monad                 (replicateM)
import           Control.Monad.Extra           (ifM)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.State           (MonadState (..), evalStateT, modify)
import           Data.Aeson                    (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Bool                     (bool)
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either.Extra             (eitherToMaybe)
import           Data.Functor                  ((<&>))
import           Data.List                     (delete, partition)
import           Data.Maybe                    (catMaybes)
import           Data.Time                     (UTCTime, getCurrentTime)
import qualified Data.Time                     as Time
import           ENCOINS.BaseTypes             (FieldElement, MintingPolarity (..))
import           ENCOINS.Bulletproofs          (BulletproofSetup, Secret (..), fromSecret)
import           ENCOINS.Core.OffChain         (EncoinsMode (..))
import           Encoins.Relay.Server.Internal (EncoinsRelayEnv (..), getEncoinsSymbol, getLedgerAddress)
import           Encoins.Relay.Server.Server   (EncoinsApi)
import           GHC.Generics                  (Generic)
import           Ledger                        (fromCardanoValue)
import           Plutus.Script.Utils.Ada       (Ada (..), lovelaceOf)
import           Plutus.V2.Ledger.Api          (TokenName (..))
import qualified Plutus.V2.Ledger.Api          as P
import           PlutusAppsExtra.IO.ChainIndex (getValueAt)
import           PlutusAppsExtra.IO.Wallet     (getWalletValue)
import qualified PlutusTx.AssocMap             as PAM
import           System.Directory              (listDirectory, removeFile)
import           System.Random                 (Random (..), randomIO, randomRIO)
import Control.Monad.Catch

type HasEncoinsModeAndBulletproofSetup = (?mode :: EncoinsMode, ?bulletproofSetup :: BulletproofSetup)

data ClientSecret = ClientSecret
    { csGamma     :: FieldElement
    , csValue     :: FieldElement
    , csPolarity  :: MintingPolarity
    , csName      :: TokenName
    , csConfirmed :: Bool
    , csCreated   :: UTCTime
    , csMode      :: EncoinsMode
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

readSecretFile :: FilePath -> IO (Maybe ClientSecret)
readSecretFile = fmap (eitherToMaybe . eitherDecode) . LBS.readFile

readSecretFiles :: (MonadIO m, HasEncoinsModeAndBulletproofSetup) => m [ClientSecret]
readSecretFiles = liftIO $ listDirectory "secrets"
    >>= mapM (readSecretFile . ("secrets/" <>)) <&> filter ((== ?mode) . csMode) . catMaybes

clientSecretToFilePath :: ClientSecret -> FilePath
clientSecretToFilePath = tokenNameToFilePath . csName

tokenNameToFilePath :: TokenName -> FilePath
tokenNameToFilePath = ("secrets/" <>) . (<> ".json") . show

secretToTokenName :: HasEncoinsModeAndBulletproofSetup => Secret -> TokenName
secretToTokenName = TokenName . snd . fromSecret ?bulletproofSetup

clientSecretToSecret :: ClientSecret -> Secret
clientSecretToSecret ClientSecret{..} = Secret csGamma csValue

mkSecretFile :: (MonadIO m, HasEncoinsModeAndBulletproofSetup) => Secret -> MintingPolarity -> m ()
mkSecretFile Secret{..} pol = do
    ct <- liftIO getCurrentTime
    let name = secretToTokenName Secret{..}
        cs   = ClientSecret secretGamma secretV pol name False ct ?mode
    liftIO $ LBS.writeFile (clientSecretToFilePath cs) $ encode cs

confirmTokens :: (HasEncoinsModeAndBulletproofSetup, MonadIO m, MonadCatch m) => AppT EncoinsApi m ()
confirmTokens = do
    tokens <- getEncoinsTokensFromMode
    (confirmMint, confirmBurn) <- partition ((== Mint) . csPolarity) . filter ((== False) . csConfirmed)
        <$> readSecretFiles
    let confirmedMint = filter ((`elem` tokens) . csName) confirmMint
        confirmedBurn = filter ((`notElem` tokens) . csName) confirmBurn
    mapM_ (confirmFile . clientSecretToFilePath) confirmedMint
    mapM_ (liftIO . removeFile . clientSecretToFilePath) confirmedBurn
    ct <- liftIO getCurrentTime
    toRemove <- filter (\ClientSecret{..} -> not csConfirmed && Time.diffUTCTime ct csCreated > Time.nominalDay)
        <$> readSecretFiles
    mapM_ (liftIO . removeFile . clientSecretToFilePath) toRemove

confirmFile :: MonadIO m => FilePath -> m ()
confirmFile fp = liftIO $ readSecretFile fp >>= \case
    Just sf -> LBS.writeFile fp $ encode sf{csConfirmed = True}
    Nothing -> removeFile fp

getEncoinsTokensFromMode :: (HasEncoinsModeAndBulletproofSetup, MonadIO m, MonadCatch m) =>  AppT EncoinsApi m [TokenName]
getEncoinsTokensFromMode = do
    encoinsSymb <- getEncoinsSymbol
    let filterCS cs tokenName = if cs == encoinsSymb then Just tokenName else Nothing
    concatMap PAM.keys . PAM.elems . PAM.mapMaybeWithKey filterCS . P.getValue <$> case ?mode of
        WalletMode -> getWalletValue
        LedgerMode -> getLedgerAddress >>= fmap fromCardanoValue . getValueAt

-- It is possible to send 5 tokens, but in this case, the size of the tx will
-- most likely not fit into the utxo size restrictions.
genTerms :: (HasEncoinsModeAndBulletproofSetup, MonadIO m) => AppT EncoinsApi m [EncoinsRequestTerm]
genTerms = do
    secrets <- map csName . filter (\ClientSecret{..} -> csPolarity == Mint && csConfirmed) <$> readSecretFiles
    l <- randomRIO (1, 4)
    if length secrets > 10
    then pure $ map (RPBurn . Right . tokenNameToFilePath) $ take l secrets
    else (`evalStateT` secrets) $ replicateM l genTerm
    where
        genTerm = get >>= \secrets -> ifM ((null secrets ||) <$> randomIO) randomMintTerm $ do
            secret <- (secrets!!) <$> (randomIO <&> (`mod` length secrets))
            modify $ delete secret
            pure $ RPBurn $ Right $ tokenNameToFilePath secret

randomMintTerm :: MonadIO m => m EncoinsRequestTerm
randomMintTerm = randomMintTermWithUB 100

randomMintTermWithUB :: MonadIO m => Integer -> m EncoinsRequestTerm
randomMintTermWithUB upperBound = randomRIO (1, upperBound) <&> RPMint . lovelaceOf

data EncoinsRequestTerm
    = RPMint Ada
    | RPBurn (Either Secret FilePath)
    deriving (Show, Eq, Generic, ToJSON)

instance Random EncoinsRequestTerm where
    random g =
        let (b, g')   = random g
            (a, g'')  = random g'
            (s, g''') = random g''
        in bool (RPMint $ Lovelace a, g'') (RPBurn $ Left s, g''') b
    randomR _ = random

instance Random [EncoinsRequestTerm] where
    random g  =
        let (reqTerms, gNew) = foldr (\_ (lst, g') -> let (e, g'') = random g' in (e:lst, g'')) ([], g) [1 :: Integer .. 5]
        in (reqTerms, gNew)
    randomR _ = random