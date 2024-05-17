module Main (main) where

import Data.Maybe (fromJust)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple (confHook, defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, configFlags, localPkgDescr)
import Distribution.Simple.Setup (ConfigFlags)
import System.Directory (getCurrentDirectory)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
           confHook = relayConfHook
       }

libsodiumLib :: String
libsodiumLib = "/../libsodium-vrf/lib"

libsodiumInclude :: String
libsodiumInclude = "/../libsodium-vrf/include"

relayConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
                  ConfigFlags ->
                  IO LocalBuildInfo
relayConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    pure localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.includeDirs = (dir <> libsodiumInclude):PD.includeDirs libraryBuildInfo,
                    PD.extraLibDirs = (dir <> libsodiumLib):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }
