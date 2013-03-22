{-#LANGUAGE NamedFieldPuns#-}

--module Main (main) where

import Control.Monad

import System.IO
import System.Environment
import System.FilePath
import System.Directory

import Distribution.Simple.Utils(cabalVersion)
import Distribution.InstalledPackageInfo(installedPackageId)

import Distribution.Package
import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.HBrew.Cabal
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Management
import Distribution.HBrew.Haddock

cabalCheck :: IO ()
cabalCheck = do
  prog <- getProgName
  cbl  <- cabalLibraryVersion
  when (cbl /= cabalVersion) $ hPutStrLn stderr $
    "Warning: Cabal library of cabal-install(" ++ showText cbl ++
    ") does't match it of " ++ prog ++ '(': showText cabalVersion ++ ")"

data Config = Config { programName       :: String
                     , confUserConfDir   :: FilePath
                     , confGlobalConfDir :: FilePath
                     , confBinDir        :: FilePath
                     , confHBrewLibDir   :: FilePath
                     , confHBrewConfDir  :: FilePath
                     , confHBrewDocDir   :: FilePath
                     }
              deriving Show


makeConfig :: IO Config
makeConfig = do
  home       <- getHomeDirectory
  pName      <- getProgName
  uConfDir   <- packageDir User
  gConfDir   <- packageDir Global
  binDir     <- createDirectoryRecursive home [".cabal", "bin"]
  hbLibdir   <- createDirectoryRecursive home [".cabal", "hbrew", "lib"]
  hbConfDir  <- createDirectoryRecursive home [".cabal", "hbrew", "conf"]
  hbDockdir  <- createDirectoryRecursive home [".cabal", "hbrew", "doc"]
  return $ Config { programName       = pName
                  , confUserConfDir   = uConfDir
                  , confGlobalConfDir = gConfDir
                  , confBinDir        = binDir
                  , confHBrewLibDir   = hbLibdir
                  , confHBrewConfDir  = hbConfDir
                  , confHBrewDocDir   = hbDockdir
                  }

configGraph :: Config -> IO Graph
configGraph Config{confGlobalConfDir, confHBrewConfDir} = do
  uConfs <- (map (confHBrewConfDir </>) . filter (`notElem` [".", ".."])) `fmap`
            getDirectoryContents confHBrewConfDir
  gConfs <- (map (confGlobalConfDir </>) . filter (`notElem` [".", "..", "package.cache"])) `fmap`
            getDirectoryContents confGlobalConfDir
  makeConfigGraph uConfs gConfs

installAction :: [String] -> Config -> [PackageId] -> IO ()
installAction args conf@Config{confUserConfDir, confHBrewConfDir, confHBrewLibDir, confBinDir} pkgs = do
  reset confUserConfDir
  toInstall <- cabalDryRun args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  push confHBrewConfDir confUserConfDir $ flatten toPush
  recache
  when (not $ null toIns) $ do
    cabalInstall confHBrewLibDir $ [ "--haddock-hyperlink-source"
                                   , "--symlink-bindir=" ++ confBinDir
                                   ] ++ map showText toIns
  pull confUserConfDir confHBrewLibDir confHBrewConfDir
  recache

haddockAction :: Config -> IO ()
haddockAction conf@Config{confHBrewDocDir} = do
  graph <- configGraph conf
  genIndex confHBrewDocDir graph

dryRunAction :: [String] -> Config -> [PackageId] -> IO ()
dryRunAction args conf@Config{confUserConfDir} pkgs = do
  reset confUserConfDir
  toInstall <- cabalDryRun args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  mapM_ (\n -> putStr "[PUSH]    " >>
               putStrLn (showText . installedPackageId $ packageInfo n)) $ flatten toPush
  mapM_ (\p -> putStr "[INSTALL] " >> putStrLn (showText p) ) toIns

preprocess :: IO Config
preprocess = cabalCheck >> makeConfig

help :: String -> String
help pName = unlines
             [ "usage: " ++ pName ++ " COMMAND [Args]"
             , "  COMMAND:"
             , "    install\tinstall package"
             , "    setup\tinstall package with --only-dependences"
             , "    dryrun [install|setup]\tdry run"
             , ""
             , "    reset\tpull all hbrew packages"
             , "    haddock\tgenerate haddock"
             ]

main :: IO ()
main = do
  args_  <- getArgs
  config <- preprocess
  case args_ of
    "install":args          -> installAction [] config $ map readText args
    "setup":  args          -> installAction ["--only-dependencies"] config $ map readText args

    "reset":_               -> reset (confUserConfDir config)
    "dryrun":"install":args -> dryRunAction [] config $ map readText args
    "dryrun":"setup":  args -> dryRunAction ["--only-dependencies"] config $ map readText args
    "haddock":_             -> haddockAction config
    _                       -> putStrLn $ help (programName config)
