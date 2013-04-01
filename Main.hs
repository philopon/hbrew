{-#LANGUAGE NamedFieldPuns, ScopedTypeVariables#-}

module Main (main) where

import Prelude hiding(catch)
import Control.Monad
import Control.Exception

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Directory
import System.Console.GetOpt

import Distribution.Simple.Utils(cabalVersion)
import Distribution.InstalledPackageInfo(installedPackageId)

import Distribution.Package
import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.HBrew.Cabal
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Management
import Distribution.HBrew.Haddock
import Distribution.HBrew.Ghc
import Distribution.Version

import Data.List

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


makeConfig :: String -> IO Config
makeConfig suf = do
  home       <- getHomeDirectory
  pName      <- getProgName
  uConfDir   <- packageDir suf User
  gConfDir   <- packageDir suf Global
  ghc        <- showGhcVersion `fmap` ghcVersion suf
  binDir     <- createDirectoryRecursive home [".cabal", "hbrew", "bin"]
  hbLibdir   <- createDirectoryRecursive home [".cabal", "hbrew", "lib",  ghc]
  hbConfDir  <- createDirectoryRecursive home [".cabal", "hbrew", "conf", ghc]
  hbDockdir  <- createDirectoryRecursive home [".cabal", "hbrew", "doc",  ghc]
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

installAction :: String -> [String] -> Config -> [PackageId] -> IO ()
installAction suf args conf@Config{confUserConfDir, confHBrewConfDir, confHBrewLibDir, confBinDir} pkgs = do
  reset suf confUserConfDir
  toInstall <- toInstallPkgs args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  push confHBrewConfDir confUserConfDir $ flatten toPush
  recache suf
  hyperLink <- (\a -> case a of
                   Just _  -> ("--haddock-hyperlink-source":)
                   Nothing -> ("--disable-documentation":)
               ) `fmap` findExecutable "haddock"
  when (not $ null toIns) $ do
    cabalInstall confHBrewLibDir (hyperLink
                                  [ "--symlink-bindir=" ++ confBinDir
                                  ] ++ map showText toIns)
      `catch` (\(_::SomeException) -> finalizer >> exitFailure)
    finalizer
    where finalizer = pull confUserConfDir confHBrewLibDir confHBrewConfDir >> recache suf

haddockAction :: Config -> IO ()
haddockAction conf@Config{confHBrewDocDir} = do
  graph <- configGraph conf
  genIndex confHBrewDocDir graph

dryRunAction :: String -> [String] -> Config -> [PackageId] -> IO ()
dryRunAction suf args conf@Config{confUserConfDir} pkgs = do
  reset suf confUserConfDir
  toInstall <- toInstallPkgs args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  mapM_ (\n -> putStr "[PUSH]    " >>
               putStrLn (showText . installedPackageId $ packageInfo n)) $ flatten toPush
  mapM_ (\p -> putStr "[INSTALL] " >> putStrLn (showText p) ) toIns

toInstallPkgs :: [String] -> [PackageId] -> IO [PackageId]
toInstallPkgs args pkgs = do
  dryrun <- cabalDryRun args pkgs
  let vPkgs = filter (not. null. versionBranch. packageVersion) pkgs
  return $ nubBy (\a b -> packageName a == packageName b) (vPkgs ++ dryrun)



data Options = Options { ghcSuffix :: String
                       , doDryRun  :: Bool
                       }
             | Help

defaultOptions :: Options
defaultOptions = Options {ghcSuffix = ""
                         , doDryRun = False
                         }

setGhcSuffix :: String -> Options -> Options
setGhcSuffix v o@Options{} = o{ghcSuffix = v}
setGhcSuffix _ Help        = Help

setDoDryRun :: Options -> Options
setDoDryRun  o@Options{} = o{doDryRun = True}
setDoDryRun  Help        = Help

options :: [OptDescr (Options -> Options)]
options =
  [ Option "h" ["help"]   (NoArg $ const Help) "show this message."
  , Option [] ["dry-run"] (NoArg $ setDoDryRun) "dry-run"
  , Option [] ["suffix"]  (ReqArg setGhcSuffix "SUF") "suffix of ghc/ghc-pkg to use."
  ]

help :: String -> String
help pName = init $ unlines
             [ "usage: " ++ pName ++ " COMMAND [Args]"
             , "  COMMAND:"
             , "    install\tinstall package"
             , "    setup\tinstall package with --only-dependences"
             , ""
             , "    reset\tpull all hbrew packages"
             , "    haddock\tgenerate haddock"
             , ""
             , "  OPTIONS:"
             ]

main :: IO ()
main = do
  args_  <- getArgs
  pName  <- getProgName
  cabalCheck
  case getOpt Permute options args_ of
    (_, [], _) -> putStrLn "command not specified." >>
                   putStrLn (usageInfo (help pName) options) >> exitFailure
    (o, n:ns, []) -> case foldl (flip id) defaultOptions o of
      Help -> putStrLn (usageInfo (help pName) options)
      Options{ghcSuffix = suf, doDryRun = dryRun} -> do
        config <- makeConfig suf
        case n of
          "install" -> (if dryRun then dryRunAction else installAction)
                       suf (cabalSuf suf) config $ map readText ns
          "setup"   -> (if dryRun then dryRunAction else installAction)
                       suf ("--only-dependencies" : cabalSuf suf) config $ map readText ns
          "reset"   -> reset suf (confUserConfDir config)
          "haddock" -> haddockAction config
          _         -> putStrLn "command not found" >> exitFailure
    (_, _, err) -> putStrLn (concat err) >> exitFailure
  where cabalSuf ""  = []
        cabalSuf suf = ["--with-ghc=ghc" ++ suf, "--with-ghc-pkg=ghc-pkg" ++ suf]

