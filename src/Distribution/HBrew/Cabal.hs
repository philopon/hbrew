
module Distribution.HBrew.Cabal
       (cabalLibraryVersion, cabalDryRun, cabalInstall, partitionPackages) where

import Distribution.HBrew.Utils

import Control.Arrow
import System.IO (hGetContents)
import System.FilePath

import System.Process (CreateProcess(..), StdStream(..), proc)

import Data.Maybe (mapMaybe)

import Distribution.Text(simpleParse)
import Distribution.Package(PackageId, PackageIdentifier(..), PackageName)
import Distribution.Version

import Data.Time(getCurrentTime, formatTime)
import System.Locale(defaultTimeLocale)
import Data.List

cabalDryRun :: String -> [String] -> [String] -> IO [PackageId]
cabalDryRun cabal args pkgs = 
  createAndWaitProcess fun
  (proc cabal $ ["install", "--dry-run", "--avoid-reinstall"] ++ args ++ pkgs){std_out = CreatePipe}

    where fun (_, Just stdout, _) = (mapMaybe simpleParse. lines) `fmap` hGetContents stdout
          fun _ = error "cabalDryRun: CreatePipe failed."

cabalLibraryVersion :: String -> IO Version
cabalLibraryVersion cabal =
  createAndWaitProcess fun (proc cabal ["--version"]){std_out = CreatePipe}
  where fun (_, Just stdout, _) =
          (last. mapMaybe simpleParse. split (`notElem` " \r\n")) `fmap` hGetContents stdout
        fun _ = error "cabalLibraryVersion: CreatePipe failed."

cabalInstall :: String -> FilePath -> [String] -> IO ()
cabalInstall cabal hbrewLibDir args = do
  time <- formatTime defaultTimeLocale "%s" `fmap` getCurrentTime
  let path = hbrewLibDir </> "$pkg/$version" </> time
  createAndWaitProcess (const $ return ())
    (proc cabal $ ["install", "--user", "--avoid-reinstall", "--prefix=" ++ path] ++ args)

type WithLibrary = String
type ProgramOnly = String
partitionPackages :: String -> [String] -> 
                     IO ([(WithLibrary, PackageName)], [(ProgramOnly, PackageName)])
partitionPackages cabal pkgs = do
  (_, Just stdout, _) <- createAndWaitProcess return (proc cabal $ "info" : pkgs){std_out = CreatePipe}
  (dropBool. partition (snd.snd). zip pkgs. map parseHeader. filter isHeader. lines) `fmap` hGetContents stdout
  where isHeader ('*':_) = True
        isHeader _       = False
        parseHeader []  = error "partitionPackages: pig fly! header line has not enough char."
        parseHeader [_] = error "partitionPackages: pig fly! header line has not enough char."
        parseHeader (_:_:l) = let (spid, cat) = span (/= ' ') l
                                  pid = readText spid
                              in (pkgName pid, "library" `isInfixOf` cat)
        dropBool = let sub = map (second fst) in sub *** sub
