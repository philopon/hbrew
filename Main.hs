{-#LANGUAGE NamedFieldPuns#-}

module Main (main) where

import Control.Monad

import Distribution.Package

import System.IO
import System.Environment
import System.Directory

import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.HBrew.Cabal
import Distribution.HBrew.DepGraph
import Distribution.Simple.Utils(cabalVersion)
import Distribution.HBrew.Management
import Distribution.HBrew.Dependency
import Distribution.HBrew.Haddock

cabalCheck :: IO ()
cabalCheck = do
  prog <- getProgName
  cbl  <- cabalLibraryVersion
  when (cbl /= cabalVersion) $ hPutStrLn stderr $
    "Warning: Cabal library of cabal-install(" ++ showText cbl ++
    ") does't match it of " ++ prog ++ '(': showText cabalVersion ++ ")"

data Config = Config { programName    :: String
                     , confPackageDir :: FilePath
                     , confLibDir     :: FilePath
                     , confBinDir     :: FilePath
                     , confHaddockDir :: FilePath
                     }
              deriving Show


setupAction :: [String] -> Config -> [PackageId] -> IO ()
setupAction args Config{confPackageDir, confLibDir, confBinDir} pkgs = do
  reset confPackageDir
  toInstalls <- cabalDryRun args pkgs
  graph      <- makeConfigGraph confLibDir 5
  let (toPush, toIns) = makeProcedure graph toInstalls
  mapM_ (push confLibDir confPackageDir) toPush
  recache
  mapM_ (cabalInstall1 [ "--haddock-hyperlink-source"
                       , "--symlink-bindir=" ++ confBinDir
                       ] confPackageDir confLibDir) toIns


haddockAction :: Config -> IO ()
haddockAction Config{confLibDir, confHaddockDir} = do
  putStrLn "Generate Haddock index file."
  graph <- makeConfigGraph confLibDir 5
  genIndex confHaddockDir graph

summaryAction :: [String] -> Config -> [PackageId] -> IO ()
summaryAction args Config{confPackageDir, confLibDir} pkgs = do
  reset confPackageDir
  toInstalls <- cabalDryRun args pkgs
  graph      <- makeConfigGraph confLibDir 5
  let (toPush, toIns) = makeProcedure graph toInstalls
  mapM_ (\p -> putStr "[PUSH]    " >> putStrLn (showText $ root p) ) toPush
  mapM_ (\p -> putStr "[INSTALL] " >> putStrLn (showText p) ) toIns

makeConfig :: IO Config
makeConfig = do
  pName  <- getProgName
  pdir   <- packageDir User
  home   <- getHomeDirectory
  libdir <- createDirectoryRecursive home [".cabal", "hbrew", "lib"]
  bindir <- createDirectoryRecursive home [".cabal", "bin"]
  haddockdir <- createDirectoryRecursive home [".cabal", "hbrew", "doc"]
  return $ Config pName pdir libdir bindir haddockdir

preprocess :: IO Config
preprocess = cabalCheck >> makeConfig

help :: String -> String
help pName = unlines
             [ "usage: " ++ pName ++ " COMMAND [Args]"
             , "  COMMAND:"
             , "    install"
             , "    setup"
             , "    reset"
             , "    summary"
             , "    haddock"
             ]

main :: IO ()
main = do
  args_  <- getArgs
  config <- preprocess
  case args_ of
    "install":args -> setupAction [] config $ map readText args
    "setup":args   -> setupAction ["--only-dependencies"] config $ map readText args
    "reset":_      -> reset (confPackageDir config)
    "summary":args -> summaryAction [] config $ map readText args
    "haddock":_    -> haddockAction config
    _              -> putStrLn $ help (programName config)
