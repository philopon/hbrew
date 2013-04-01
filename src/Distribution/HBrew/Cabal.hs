
module Distribution.HBrew.Cabal(cabalLibraryVersion, cabalDryRun, cabalInstall) where

import Distribution.HBrew.Utils

import System.IO (hGetContents)
import System.FilePath

import System.Process (CreateProcess(..), StdStream(..), proc)

import Data.Maybe (catMaybes)

import Distribution.Text(simpleParse)
import Distribution.Package(PackageId)
import Distribution.Version

import Data.Time(getCurrentTime, formatTime)
import System.Locale(defaultTimeLocale)

cabalDryRun :: String -> [String] -> [PackageId] -> IO [PackageId]
cabalDryRun cabal args pkgs = do
  createAndWaitProcess fun
    (proc cabal $ ["install", "--dry-run", "--avoid-reinstall"] ++ args ++ map showText pkgs
    ){std_out = CreatePipe}

    where fun (_, Just stdout, _) = hGetContents stdout >>=
                                    return. catMaybes. map simpleParse. lines
          fun _ = error "cabalDryRun: CreatePipe failed."

cabalLibraryVersion :: String -> IO Version
cabalLibraryVersion cabal = do
  createAndWaitProcess fun (proc cabal ["--version"]){std_out = CreatePipe}
  where fun (_, Just stdout, _) =
          (last. catMaybes. map simpleParse. split (`notElem` " \r\n")) `fmap` hGetContents stdout
        fun _ = error "cabalLibraryVersion: CreatePipe failed."

cabalInstall :: String -> FilePath -> [String] -> IO ()
cabalInstall cabal hbrewLibDir args = do
  time <- formatTime defaultTimeLocale "%s" `fmap` getCurrentTime
  let path = hbrewLibDir </> "$pkg/$version" </> time
  createAndWaitProcess (const $ return ())
    (proc cabal $ ["install", "--user", "--avoid-reinstall", "--prefix=" ++ path] ++ args)

