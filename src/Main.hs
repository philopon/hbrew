{-#LANGUAGE NamedFieldPuns, ScopedTypeVariables#-}

module Main (main) where

import Prelude hiding(catch)
import Control.Monad
import Control.Applicative
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
import Data.Function

cabalCheck :: String -> IO ()
cabalCheck cabal = do
  prog <- getProgName
  cbl  <- cabalLibraryVersion cabal
  when (cbl /= cabalVersion) $ hPutStrLn stderr $
    "Warning: Cabal library of cabal-install(" ++ showText cbl ++
    ") doesn't match it of " ++ prog ++ '(': showText cabalVersion ++ ")"

data Config = Config { programName       :: String
                     , confUserConfDir   :: FilePath
                     , confGlobalConfDir :: FilePath
                     , confBinDir        :: FilePath
                     , confHBrewLibDir   :: FilePath
                     , confHBrewConfDir  :: FilePath
                     , confHBrewDocDir   :: FilePath
                     }
              deriving Show


makeConfig :: Options -> IO Config
makeConfig Help = error "makeConfig: pig fly"
makeConfig Options{ghc, ghcPkg} = do
  home       <- getHomeDirectory
  pName      <- getProgName
  uConfDir   <- packageDir ghcPkg User
  gConfDir   <- packageDir ghcPkg Global
  ghcVer     <- showGhcVersion `fmap` ghcVersion ghc
  binDir     <- createDirectoryRecursive home [".cabal", "hbrew", "bin"]
  hbLibdir   <- createDirectoryRecursive home [".cabal", "hbrew", "lib",  ghcVer]
  hbConfDir  <- createDirectoryRecursive home [".cabal", "hbrew", "conf", ghcVer]
  hbDockdir  <- createDirectoryRecursive home [".cabal", "hbrew", "doc",  ghcVer]
  return Config { programName       = pName
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

installAction :: Options -> [String] -> Config -> [PackageId] -> IO ()
installAction Help _ _ _ = error "installAction: pig fly"
installAction Options{ghcPkg, cabal, verbosity} args
  conf@Config{confUserConfDir, confHBrewConfDir, confHBrewLibDir, confBinDir} pkgs = do
  reset ghcPkg confUserConfDir
  toInstall <- toInstallPkgs cabal args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  push confHBrewConfDir confUserConfDir $ flatten toPush
  recache ghcPkg
  unless (null toIns) $ do
    let opts = ("--haddock-hyperlink-source":
                ("--symlink-bindir=" ++ confBinDir):
                args ++ map showText pkgs)
    when (verbosity > Right 0) $ print opts
    cabalInstall cabal confHBrewLibDir opts
      `catch` (\(_::SomeException) -> finalizer >> exitFailure)
    finalizer
    where finalizer = pull confUserConfDir confHBrewLibDir confHBrewConfDir >> recache ghcPkg

haddockAction :: String -> Config -> IO ()
haddockAction haddock conf@Config{confHBrewDocDir} = do
  graph <- configGraph conf
  genIndex haddock confHBrewDocDir graph

dryRunAction :: Options -> [String] -> Config -> [PackageId] -> IO ()
dryRunAction Help _ _ _ = error "dryRunAction: pig fly"
dryRunAction Options{ghcPkg,cabal} args conf@Config{confUserConfDir} pkgs = do
  reset ghcPkg confUserConfDir
  toInstall <- toInstallPkgs cabal args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  mapM_ (\n -> putStr "[PUSH]    " >>
               putStrLn (showText . installedPackageId $ packageInfo n)) $ flatten toPush
  mapM_ (\p -> putStr "[INSTALL] " >> putStrLn (showText p) ) toIns

toInstallPkgs :: String -> [String] -> [PackageId] -> IO [PackageId]
toInstallPkgs cabal args pkgs = do
  dryrun <- cabalDryRun cabal args pkgs
  let vPkgs = filter (not. null. versionBranch. packageVersion) pkgs
  return $ nubBy ((==) `on` packageName) (vPkgs ++ dryrun)

data Options = Options { ghc        :: String
                       , ghcPkg     :: String
                       , haddock    :: String
                       , cabal      :: String
                       , doDryRun   :: Bool
                       , verbosity  :: Either String Int
                       }
             | Help

defaultOptions :: Options
defaultOptions = Options { ghc = "ghc"
                         , ghcPkg = "ghc-pkg"
                         , haddock = "haddock"
                         , cabal   = "cabal"
                         , doDryRun = False
                         , verbosity = Right 0
                         }

setGhc :: String -> Options -> Options
setGhc v o@Options{} = o{ghc = v}
setGhc _ Help        = Help

setGhcPkg :: String -> Options -> Options
setGhcPkg v o@Options{} = o{ghcPkg = v}
setGhcPkg _ Help        = Help

setHaddock :: String -> Options -> Options
setHaddock v o@Options{} = o{haddock = v}
setHaddock _ Help        = Help

setCabal :: String -> Options -> Options
setCabal v o@Options{} = o{cabal = v}
setCabal _ Help        = Help

setDoDryRun :: Options -> Options
setDoDryRun  o@Options{} = o{doDryRun = True}
setDoDryRun  Help        = Help

setVerbosity :: Maybe String -> Options -> Options
setVerbosity (Just v) o@Options{} = o{verbosity = Left v}
setVerbosity Nothing  o@Options{} = o{verbosity = Right 1}
setVerbosity _        Help        = Help

options :: [OptDescr (Options -> Options)]
options =
  [ Option "h" ["help"]   (NoArg $ const Help) "show this message."
  , Option [] ["dry-run"] (NoArg setDoDryRun) "dry-run"
  , Option [] ["with-ghc"]  (ReqArg setGhc "PATH") "ghc program name."
  , Option [] ["with-ghc-pkg"]  (ReqArg setGhcPkg "PATH") "ghc-pkg program name."
  , Option [] ["with-haddock"]  (ReqArg setHaddock "PATH") "haddock program name."
  , Option [] ["with-cabal"]  (ReqArg setCabal "PATH") "cabal program name."
  , Option "v" ["verbose"]  (OptArg setVerbosity "INT") "verbose level[0-1]"
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

programCheck :: Options -> IO (Maybe IOError)
programCheck Help = error "programCheck: pig fly"
programCheck Options{ghc,ghcPkg,haddock,cabal} = do
  _ <- toEither "ghc not found."     <$> findExecutable ghc
  _ <- toEither "ghc-pkg not found." <$> findExecutable ghcPkg
  _ <- toEither "cabal not found."   <$> findExecutable cabal
  toMaybe . toEither "haddock not found." <$> findExecutable haddock
  where toEither msg Nothing  = Left (userError msg)
        toEither _   (Just a) = Right a
        toMaybe (Left msg) = Just msg
        toMaybe (Right _)  = Nothing

main :: IO ()
main = do
  args_  <- getArgs
  pName  <- getProgName
  let showHelp []  = putStrLn (usageInfo (help pName) options) >> exitSuccess
      showHelp err = do hPutStrLn stderr err
                        putStrLn (usageInfo (help pName) options)
                        exitFailure
  case getOpt Permute options args_ of
    (_, [], _) -> showHelp "command not specified."
    (o, n:ns, []) -> case foldl (flip id) defaultOptions o of
      Help -> showHelp ""
      opts'@Options{doDryRun, cabal, ghc, ghcPkg, haddock, verbosity = v'} -> do
        let opts = opts'{verbosity = Right $ either read id v'}
        maybe (return ()) throwIO =<< programCheck opts
        cabalCheck cabal
        config <- makeConfig opts
        let cabalOpts = ["--with-ghc=" ++ ghc
                        , "--with-ghc-pkg=" ++ ghcPkg
                        , "--with-haddock=" ++ haddock]
        case n of
          "install" -> (if doDryRun then dryRunAction else installAction)
                       opts cabalOpts config $ map readText ns
          "setup"   -> (if doDryRun then dryRunAction else installAction)
                       opts ("--only-dependencies": cabalOpts) config $ map readText ns
          "reset"   -> reset ghcPkg (confUserConfDir config)
          "haddock" -> haddockAction haddock config
          _         -> showHelp "command not found"
    (_, _, err) -> showHelp (concat err)


