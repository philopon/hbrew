{-#LANGUAGE NamedFieldPuns, ScopedTypeVariables#-}

module Main (main) where

import Prelude
import Control.Monad
import Control.Applicative
import Control.Exception as E

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Directory
import System.Console.GetOpt

import Distribution.Simple.Utils(cabalVersion)
import Distribution.InstalledPackageInfo(InstalledPackageInfo,
                                         installedPackageId, sourcePackageId, importDirs)
import Distribution.Package

import Distribution.HBrew.Compatibility
import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.HBrew.Cabal
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Management
import Distribution.HBrew.Haddock
import Distribution.HBrew.Ghc

import Data.Maybe
import Data.Char
import Data.List

configGraph :: Config -> IO Graph
configGraph Config{confGlobalConfDir, confHBrewConfDir} = do
  uConfs <- (map (confHBrewConfDir  </>) . filter (`notElem` [".", ".."])) <$>
            getDirectoryContents confHBrewConfDir
  gConfs <- (map (confGlobalConfDir </>) . filter (`notElem` [".", "..", "package.cache"])) <$>
            getDirectoryContents confGlobalConfDir
  makeConfigGraph uConfs gConfs

isInstalled :: FilePath -> PackageId -> IO Bool
isInstalled confHBrewLibDir pid =
  doesDirectoryExist $ confHBrewLibDir </> showText (pkgName pid) </> showText (pkgVersion pid)

installCommon :: Config -> [String] -> [String]
                 -> IO ([String], [String], [PackageIdentifier], Graph, [PackageId])
installCommon conf@Config{ghcPkg, cabal, confUserConfDir, confHBrewLibDir} args pkgs = do
  reset ghcPkg confUserConfDir
  toInstall    <- cabalDryRun cabal args pkgs
  graph     <- configGraph conf
  
  (lib, pOnly) <- partitionPackages cabal pkgs
  let pOnlyIds = filter (\pid -> pkgName pid `elem` map snd pOnly) toInstall
  installedPrograms <- filterM (isInstalled confHBrewLibDir) pOnlyIds
  let toInsPrograms = filter (\(_,p) -> p `notElem` map pkgName installedPrograms) pOnly

  let toInstall' = filter (`notElem` installedPrograms) toInstall
      (toPush, toIns) = makeProcedure graph toInstall'
  return (map fst toInsPrograms, map fst lib, installedPrograms, toPush, toIns)

installAction :: Config -> [String] -> [String] -> IO ()
installAction conf@Config{ ghcPkg, cabal, verbosity
                         , confUserConfDir, confHBrewConfDir, confHBrewLibDir} args pkgs = do
  (prog, lib, _, toPush, toIns) <- installCommon conf args pkgs
  push confHBrewConfDir confUserConfDir $ flatten toPush
  recache ghcPkg
  unless (null toIns) $ do
    let opts = args ++ prog ++ lib
    when (verbosity > 0) $ print opts
    cabalInstall cabal confHBrewLibDir opts
      `E.catch` (\(_::SomeException) -> finalizer (not $ null prog) >> exitFailure)
    finalizer $ not (null prog)
    where finalizer dolink = do pull confUserConfDir confHBrewLibDir confHBrewConfDir
                                when dolink $ programLinkAction True conf
                                recache ghcPkg

dryRunAction :: Config -> [String] -> [String] -> IO ()
dryRunAction conf args pkgs = do
  (_, _, installed, toPush, toIns) <- installCommon conf args pkgs
  mapM_ (\p -> putStr "[EXISTS]  " >> putStrLn (showText p) ) installed
  mapM_ (\n -> putStr "[PUSH]    " >>
               putStrLn (showText . installedPackageId $ packageInfo n)) (flatten toPush)
  mapM_ (\p -> putStr "[INSTALL] " >> putStrLn (showText p) ) toIns

haddockAction :: String -> Config -> IO ()
haddockAction haddock conf@Config{confHBrewDocDir} = do
  graph <- configGraph conf
  genIndex haddock confHBrewDocDir graph

programListAction :: Config -> IO ()
programListAction Config{confHBrewLibDir} = do
  ps <- programs confHBrewLibDir
  let maxlen = foldl' (\a (p,_) -> max a (length $ showText p)) 0 ps
  forM_ ps $ \(p,vs) -> do
    let len = length $ showText p
    putStr (showText p) >> putStr (replicate (maxlen - len + 2) ' ')
    forM_ (init vs) (\(v,_) -> putStr (showText v) >> putStr ", ")
    putStr (showText . fst $ last vs)
    putStrLn ""

cleanBinDir :: FilePath -> IO ()
cleanBinDir confHBrewBinDir = do
  links <- filter (`notElem` [".", ".."]) <$> getDirectoryContents confHBrewBinDir
  mapM_ (unShortcut . (confHBrewBinDir </>)) links

programLinkAction :: Bool -> Config -> IO ()
programLinkAction quiet Config{confHBrewBinDir, confHBrewLibDir} = do
  ps <- programs confHBrewLibDir
  cleanBinDir confHBrewBinDir
  forM_ ps $ \(pkg,vs) ->
    forM_ (zip (sortBy (\(a,_) (b,_) -> b `compare` a) vs) (True:repeat False)) $ \((v, ids), isMax) -> do
      let binDir   = confHBrewLibDir </> showText pkg </> showText v </> head ids </> "bin"
      mapM_ (\p -> do shortcut (binDir </> p) (confHBrewBinDir </> p ++ '-': showText v)
                      when isMax $ shortcut (binDir </> p) (confHBrewBinDir </> p)
            ) =<< filter (`notElem` [".", ".."]) <$> getDirectoryContents binDir
      unless quiet $ putStr "Linking " >> putStr (showText pkg) >> putChar '-' >> putStrLn (showText v)

getUniqueId :: FilePath -> InstalledPackageInfo -> Maybe String
getUniqueId confHBrewConfDir pInfo =
  let pid = sourcePackageId pInfo
      base   = splitDirectories $
               confHBrewConfDir </> showText (packageName pid) </> showText (packageVersion pid)
      target = map splitDirectories (importDirs pInfo)
  in listToMaybe $ mapMaybe (go base) target
  where go [] (b:_) = Just b
        go _ []     = Nothing
        go (a:as) (b:bs) = if a == b then go as bs else Nothing

removeLibraryAction :: Config -> [String] -> IO ()
removeLibraryAction conf@Config{confHBrewLibDir} pkgs = do
  graph     <- configGraph conf
  let ps   = map readText pkgs :: [PackageId]
      base = concatMap (`lookupNodesFuzzy` graph) ps
      rms  = nub. flatten $ ancestor graph base
  mapM_ (putStrLn. showText . installedPackageId . packageInfo) rms
  putStr "delete " >> putStr (show $ length rms) >> putStrLn " packages. continue? (y/n)"
  when (null rms) $ fail "not match"
  confirm
  forM_ rms $ \node -> do
    --removeFile $ cabalFile node
    let mbuid  = getUniqueId confHBrewLibDir (packageInfo node)
        pId    = sourcePackageId $ packageInfo node
        pkgdir = confHBrewLibDir </> showText (packageName pId) </> showText (packageVersion pId)
    case mbuid of
      Nothing  -> putStr "cannot get unique id of " >> putStr (showText pId)
      Just uid -> do removeFile $ cabalFile node
                     removeDirectoryRecursive $ pkgdir </> uid
  where confirm = getChar >>= \c -> case toLower c of
          'y' -> return ()
          'n' -> fail "interrupt"
          _   -> putStr "(y/n)" >> confirm


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
                     , confHBrewBinDir   :: FilePath
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

set f o@RawOptions{} = f o
set f Help           = Help

setVerbosity :: Maybe String -> RawOptions -> RawOptions
setVerbosity (Just v) o@RawOptions{} = o{verbosity' = v}
setVerbosity Nothing  o@RawOptions{} = o{verbosity' = "1"}
setVerbosity _        Help           = Help

options :: [OptDescr (RawOptions -> RawOptions)]
options =
  [ Option "h" ["help"]              (NoArg $ const Help)                                 "show this message."
  , Option []  ["dry-run"]           (NoArg $ set (\o -> o{doDryRun'    = True}))         "dry-run"
  , Option []  ["with-ghc"]          (ReqArg (\v -> set (\o -> o{ghc'      = v})) "PATH") "ghc program name."
  , Option []  ["with-ghc-pkg"]      (ReqArg (\v -> set (\o -> o{ghcPkg'   = v})) "PATH") "ghc-pkg program name."
  , Option []  ["with-haddock"]      (ReqArg (\v -> set (\o -> o{haddock'  = v})) "PATH") "haddock program name."
  , Option []  ["with-cabal"]        (ReqArg (\v -> set (\o -> o{cabal'    = v})) "PATH") "cabal program name."
  , Option []  ["with-hscolour"]     (ReqArg (\v -> set (\o -> o{hsColour' = v})) "PATH") "HsColour program name."
  , Option "v" ["verbose"]           (OptArg setVerbosity "INT")                          "verbose level[0-1]"
  ]

showHelp :: String -> IO b
showHelp errs = do
  pName <- getProgName
  unless (null errs) $ hPutStrLn stderr errs
  putStrLn (usageInfo (prefix pName) options)
  if null errs then exitSuccess else exitFailure
  where prefix pName =
          init $ unlines
          [ "usage: " ++ pName ++ " [OPTIONS] COMMAND"
          , "  COMMAND:"
          , "    install PKG1 [PKG2..] -- [CABAL OPTS]      install package"
          , "    setup   PKG1 [PKG2..] -- [CABAL OPTS]      install package with --only-dependences"
          , "    remove  PKG1 [PKG2..]                      remove libraries from hbrew"
          , ""
          , "    reset                                      pull all hbrew packages"
          , "    haddock                                    generate haddock"
          , ""
          , "    program list                               list executable files."
          , "    program link                               link programs to hbrew/bin directory."
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
parseOptions args = 
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
        ghcVer     <- showGhcVersion <$> ghcVersion ghc'
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
                          , confHBrewBinDir   = binDir
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
  (conf@Config{ghc, ghcPkg, haddock, hsColour, doDryRun, cabal}, opts, cmds) <-
    getArgs >>= parseOptions
  cabalCheck cabal
  let cabalOpts = (case hsColour of
                      Just hsc -> (["--with-hscolour=" ++ hsc, "--haddock-hyperlink-source"] ++)
                      Nothing  -> id)
                  [ "--with-ghc="     ++ ghc
                  , "--with-ghc-pkg=" ++ ghcPkg
                  , "--with-haddock=" ++ haddock
                  ] ++ opts
  case cmds of
    "install":pkgs        -> (if doDryRun then dryRunAction else installAction)
                             conf cabalOpts pkgs
    "setup":pkgs          -> (if doDryRun then dryRunAction else installAction)
                             conf ("--only-dependencies": cabalOpts) pkgs
    "reset":_             -> reset ghcPkg (confUserConfDir conf)
    "haddock":_           -> haddockAction haddock conf
    "program":"list":_    -> programListAction conf
    "program":"link":_    -> programLinkAction False conf
    "remove":pkgs         -> removeLibraryAction conf pkgs
    _                     -> showHelp "command not found"


