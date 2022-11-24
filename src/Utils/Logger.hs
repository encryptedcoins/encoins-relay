{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Utils.Logger where

import           Control.Exception      (handle, throw)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Time              as Time
import           GHC.IO.Exception       (IOException(..), IOErrorType(NoSuchThing))
import           Prettyprinter          (Pretty(..))
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath.Posix  (takeDirectory)

class MonadIO m => HasLogger m where
    
    loggerFilePath :: FilePath

    logMsg :: Text -> m ()
    logMsg msg = liftIO $ logMsgIO msg $ loggerFilePath @m

instance HasLogger IO where

    loggerFilePath = ""
    
    logMsg = T.putStrLn

logSmth :: forall a m. (HasLogger m, Show a) => a -> m ()
logSmth a = logMsg $ T.pack $ show a

logPretty :: forall a m. (HasLogger m, Pretty a) => a -> m ()
logPretty a = logMsg $ T.pack $ show $ pretty a

logMsgIO :: Text -> FilePath -> IO ()
logMsgIO msg fileName = handle (handler msg fileName) $ do
    utcTime <- Time.getCurrentTime
    let localTime = Time.addUTCTime (10800 :: Time.NominalDiffTime) utcTime
        asctime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
        msg' = "\n" <> T.pack asctime <> " " <> "\n" <> msg <> "\n"
    T.putStrLn msg'
    T.appendFile (mkFullPath fileName) msg'

handler :: Text -> FilePath -> IOException -> IO ()
handler msg fileName err 
    | ioe_type err == NoSuchThing = do
        createDirectoryIfMissing True $ takeDirectory $ mkFullPath fileName
        logMsgIO msg fileName
    | otherwise = throw err

mkFullPath :: FilePath -> FilePath
mkFullPath = ("logs/" <>)

(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> "\n" <> T.pack (show a)

infixr 7 .<