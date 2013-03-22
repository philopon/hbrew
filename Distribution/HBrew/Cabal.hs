{-#LANGUAGE NoMonomorphismRestriction#-}
module Distribution.HBrew.Cabal(cabalLibraryVersion, cabalDryRun, cabalInstall1) where

import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.HBrew.Compatibility

import Control.Monad((<=<))
import System.IO (hGetContents)
import System.FilePath
import System.Directory
import Control.Monad(foldM)

import System.Process (CreateProcess(..), StdStream(..), proc)

import Data.Maybe (catMaybes)
import Data.List

import Distribution.Text(simpleParse)
import Distribution.Package(PackageId)
import Distribution.Version


import Data.Time(getCurrentTime, formatTime)
import System.Locale(defaultTimeLocale)

cabal :: [String] -> CreateProcess
cabal  = proc "cabal"

cabalDryRun :: [String] -> [PackageId] -> IO [PackageId]
cabalDryRun args pkgs = do
  createAndWaitProcess fun
    (cabal $ ["install", "--dry-run", "--avoid-reinstall"] ++ args ++ map showText pkgs
    ){std_out = CreatePipe}

    where fun (_, Just stdout, _) = hGetContents stdout >>=
                                    return. catMaybes. map simpleParse. lines
          fun _ = error "cabalDryRun: CreatePipe failed."

cabalLibraryVersion :: IO Version
cabalLibraryVersion = do
  createAndWaitProcess fun (cabal ["--version"]){std_out = CreatePipe}
  where fun (_, Just stdout, _) =
          (last. catMaybes. map simpleParse. split (`notElem` " \r\n")) `fmap` hGetContents stdout
        fun _ = error "cabalLibraryVersion: CreatePipe failed."

cabalInstall1 :: [String] -> FilePath -> FilePath -> PackageId -> IO (Maybe FilePath)
cabalInstall1 args pkgDir hbrewDir pkg = do
  time <- formatTime defaultTimeLocale "%s" `fmap` getCurrentTime
  let path = prefix time
  inst <- createAndWaitProcess (fun time)
          (cabal $ ["install", pkgName, "--user", "--avoid-reinstall", "--prefix=" ++ path] ++ args){
    std_out = CreatePipe }

  lconf <- (filter (pkgName `isPrefixOf`)) `fmap` getDirectoryContents pkgDir
  case lconf of
    []     -> return Nothing -- Executable Only
    conf:_ -> do renameFile (pkgDir </> conf) (inst </> conf)
                 link (inst </> conf) (pkgDir </> replaceExtension conf ".hbrew.conf")
                 recache
                 return $ Just inst

    where prefix t     = hbrewDir </> "$pkg/$version/$compiler-$arch" </> t
          pkgName      = showText pkg
          fun time (_, Just stdout, _) = installDir time stdout
          fun _     _                  = error "cabalInstall1: CreatePipe failed."
          installDir time h = (return. (</> time). joinPath. takeWhile (/= time). splitDirectories.
                               maybe (error "cabalInstall1: internal error.") id <=<
                               foldM sub Nothing. lines) =<< hGetContents h
            where sub Nothing   line = putStrLn line >> return (if line == "Installing library in"
                                                                then Just []
                                                                else Nothing)
                  sub (Just []) line = putStrLn line >> return (Just line)
                  sub dir       line = putStrLn line >> return dir

