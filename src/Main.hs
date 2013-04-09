{-#LANGUAGE NamedFieldPuns, ScopedTypeVariables#-}

module Main (main) where

import Prelude
import Control.Monad
import Control.Exception as E

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Directory
import System.Console.GetOpt

import Distribution.Simple.Utils(cabalVersion)
import Distribution.InstalledPackageInfo(installedPackageId)

import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.HBrew.Cabal
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Management
import Distribution.HBrew.Haddock
import Distribution.HBrew.Ghc

import Data.Maybe
import Data.List


configGraph :: Config -> IO Graph
configGraph Config{confGlobalConfDir, confHBrewConfDir} = do
  uConfs <- (map (confHBrewConfDir  </>) . filter (`notElem` [".", ".."])) `fmap`
            getDirectoryContents confHBrewConfDir
  gConfs <- (map (confGlobalConfDir </>) . filter (`notElem` [".", "..", "package.cache"])) `fmap`
            getDirectoryContents confGlobalConfDir
  makeConfigGraph uConfs gConfs

installAction :: Config -> [String] -> [String] -> IO ()
installAction conf@Config{ ghcPkg, cabal, verbosity
                         , confUserConfDir, confHBrewConfDir, confHBrewLibDir} args pkgs = do
  reset ghcPkg confUserConfDir
  toInstall <- cabalDryRun cabal args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  push confHBrewConfDir confUserConfDir $ flatten toPush
  recache ghcPkg
  unless (null toIns) $ do
    let opts = args ++ pkgs
    when (verbosity > 0) $ print opts
    cabalInstall cabal confHBrewLibDir opts
      `E.catch` (\(_::SomeException) -> finalizer >> exitFailure)
    finalizer
    where finalizer = pull confUserConfDir confHBrewLibDir confHBrewConfDir >> recache ghcPkg

dryRunAction :: Config -> [String] -> [String] -> IO ()
dryRunAction conf@Config{ghcPkg,cabal,confUserConfDir} args pkgs = do
  reset ghcPkg confUserConfDir
  toInstall <- cabalDryRun cabal args pkgs
  graph     <- configGraph conf
  let (toPush, toIns) = makeProcedure graph toInstall
  mapM_ (\n -> putStr "[PUSH]    " >>
               putStrLn (showText . installedPackageId $ packageInfo n)) (flatten toPush)
  mapM_ (\p -> putStr "[INSTALL] " >> putStrLn (showText p) ) toIns

haddockAction :: String -> Config -> IO ()
haddockAction haddock conf@Config{confHBrewDocDir} = do
  graph <- configGraph conf
  genIndex haddock confHBrewDocDir graph

data RawOptions = RawOptions { ghc'       :: String
                             , ghcPkg'    :: String
                             , haddock'   :: String
                             , cabal'     :: String
                             , hsColour'  :: String
                             , doDryRun'  :: Bool
                             , verbosity' :: String
                             }
                | Help

data Config = Config { ghc               :: String
                     , ghcPkg            :: String
                     , haddock           :: String
                     , cabal             :: String
                     , hsColour          :: Maybe String
                     , doDryRun          :: Bool
                     , verbosity         :: Int
                     , programName       :: String
                     , confUserConfDir   :: FilePath
                     , confGlobalConfDir :: FilePath
                     , confBinDir        :: FilePath
                     , confHBrewLibDir   :: FilePath
                     , confHBrewConfDir  :: FilePath
                     , confHBrewDocDir   :: FilePath
                     }
            deriving Show

defaultOptions :: RawOptions
defaultOptions = RawOptions { ghc'       = "ghc"
                            , ghcPkg'    = "ghc-pkg"
                            , haddock'   = "haddock"
                            , cabal'     = "cabal"
                            , hsColour'  = "HsColour"
                            , doDryRun'  = False
                            , verbosity' = "0"
                            }

setGhc :: String -> RawOptions -> RawOptions
setGhc v o@RawOptions{} = o{ghc' = v}
setGhc _ Help           = Help

setGhcPkg :: String -> RawOptions -> RawOptions
setGhcPkg v o@RawOptions{} = o{ghcPkg' = v}
setGhcPkg _ Help           = Help

setHaddock :: String -> RawOptions -> RawOptions
setHaddock v o@RawOptions{} = o{haddock' = v}
setHaddock _ Help           = Help

setCabal :: String -> RawOptions -> RawOptions
setCabal v o@RawOptions{} = o{cabal' = v}
setCabal _ Help           = Help

setHsColour :: String -> RawOptions -> RawOptions
setHsColour v o@RawOptions{} = o{hsColour' = v}
setHsColour _ Help           = Help

setDoDryRun :: RawOptions -> RawOptions
setDoDryRun  o@RawOptions{} = o{doDryRun' = True}
setDoDryRun  Help           = Help

setVerbosity :: Maybe String -> RawOptions -> RawOptions
setVerbosity (Just v) o@RawOptions{} = o{verbosity' = v}
setVerbosity Nothing  o@RawOptions{} = o{verbosity' = "1"}
setVerbosity _        Help           = Help

options :: [OptDescr (RawOptions -> RawOptions)]
options =
  [ Option "h" ["help"]          (NoArg $ const Help)         "show this message."
  , Option []  ["dry-run"]       (NoArg  setDoDryRun)         "dry-run"
  , Option []  ["with-ghc"]      (ReqArg setGhc       "PATH") "ghc program name."
  , Option []  ["with-ghc-pkg"]  (ReqArg setGhcPkg    "PATH") "ghc-pkg program name."
  , Option []  ["with-haddock"]  (ReqArg setHaddock   "PATH") "haddock program name."
  , Option []  ["with-cabal"]    (ReqArg setCabal     "PATH") "cabal program name."
  , Option []  ["with-hscolour"] (ReqArg setHsColour  "PATH") "HsColour program name."
  , Option "v" ["verbose"]       (OptArg setVerbosity "INT")  "verbose level[0-1]"
  ]

showHelp :: [Char] -> IO b
showHelp errs = do
  pName <- getProgName
  when (not $ null errs) $ hPutStrLn stderr errs
  putStrLn (usageInfo (prefix pName) options)
  if null errs then exitSuccess else exitFailure
  where prefix pName = init $ unlines
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

cabalCheck :: String -> IO ()
cabalCheck cabal = do
  prog <- getProgName
  cbl  <- cabalLibraryVersion cabal
  when (cbl /= cabalVersion) $ hPutStrLn stderr $
    "Warning: Cabal library of cabal-install(" ++ showText cbl ++
    ") doesn't match it of " ++ prog ++ '(': showText cabalVersion ++ ")"

parseOptions :: [String] -> IO (Config, [String], [String])
parseOptions args = do
  case getOpt Permute options args of
    (_, [], _)     -> showHelp "command not specified."
    (f, cs, [])    -> case foldl (flip id) defaultOptions f of
      Help         -> showHelp []
      RawOptions{ghc', ghcPkg', haddock', cabal', hsColour', doDryRun', verbosity'} -> do
        verb <- readIO verbosity'
        maybeUnlessM (findExecutable ghc')     $ throwIO (userError "ghc not found.")
        maybeUnlessM (findExecutable ghcPkg')  $ throwIO (userError "ghc-pkg not found.")
        maybeUnlessM (findExecutable haddock') $ throwIO (userError "haddock not found.")
        maybeUnlessM (findExecutable cabal')   $ throwIO (userError "cabal not found.")
        hsColour <- findExecutable hsColour' >>= \e -> return $ if isJust e then Just hsColour' else Nothing

        home       <- getHomeDirectory
        pName      <- getProgName
        uConfDir   <- packageDir ghcPkg' User
        gConfDir   <- packageDir ghcPkg' Global
        ghcVer     <- showGhcVersion `fmap` ghcVersion ghc'
        binDir     <- createDirectoryRecursive home [".cabal", "hbrew", "bin"]
        hbLibdir   <- createDirectoryRecursive home [".cabal", "hbrew", "lib",  ghcVer]
        hbConfDir  <- createDirectoryRecursive home [".cabal", "hbrew", "conf", ghcVer]
        hbDockdir  <- createDirectoryRecursive home [".cabal", "hbrew", "doc",  ghcVer]
        let conf = Config { ghc       = ghc'
                          , ghcPkg    = ghcPkg'
                          , haddock   = haddock'
                          , cabal     = cabal'
                          , hsColour  = hsColour
                          , doDryRun  = doDryRun'
                          , verbosity = verb
                          , programName       = pName
                          , confUserConfDir   = uConfDir
                          , confGlobalConfDir = gConfDir
                          , confBinDir        = binDir
                          , confHBrewLibDir   = hbLibdir
                          , confHBrewConfDir  = hbConfDir
                          , confHBrewDocDir   = hbDockdir
                          }
        let (opts, cmds) = partition (\arg -> case arg of
                                         '-':_ -> True
                                         _     -> False
                                     ) cs
        return (conf, opts, cmds)
    (_, _, err) -> showHelp (concat err)
  where maybeUnlessM p f = p >>= \r -> unless (isJust r) f

main :: IO ()
main = do
  (conf@Config{ghc, ghcPkg, haddock, hsColour, doDryRun, cabal, confBinDir}, opts, cmd:pkgs) <-
    getArgs >>= parseOptions
  cabalCheck cabal
  let cabalOpts = (case hsColour of
                      Just hsc -> (["--with-hscolour=" ++ hsc, "--haddock-hyperlink-source"] ++)
                      Nothing  -> id)
                  [ "--with-ghc="     ++ ghc
                  , "--with-ghc-pkg=" ++ ghcPkg
                  , "--with-haddock=" ++ haddock
                  , "--symlink-bindir=" ++ confBinDir
                  ]
  case cmd of
    "install" -> (if doDryRun then dryRunAction else installAction)
                 conf cabalOpts pkgs
    "setup"   -> (if doDryRun then dryRunAction else installAction)
                 conf ("--only-dependencies": cabalOpts) pkgs
    "reset"   -> reset ghcPkg (confUserConfDir conf)
    "haddock" -> haddockAction haddock conf
    _         -> showHelp "command not found"

