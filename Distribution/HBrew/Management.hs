{-#LANGUAGE TupleSections#-}
module Distribution.HBrew.Management(push, pull, reset) where

import Control.Exception
import Control.Monad

import System.IO.Error
import System.FilePath
import System.Directory
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Compatibility
import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.InstalledPackageInfo
import Data.List

push :: FilePath -> FilePath -> [Node] -> IO ()
push hbrewDir pDir nodes =
  pushFiles pDir (map ((hbrewDir </>). cabalFile) $ filter isUserPkg nodes)

pushFiles :: FilePath -> [FilePath] -> IO ()
pushFiles ucDir files = do
  mapM_ (\f -> do let target = ucDir </> takeBaseName f <.> "hbrew.conf"
                  e <- doesFileExist target
                  when (not e) $ link f target
        ) files

pull :: FilePath -> FilePath -> FilePath -> IO ()
pull uConfDir libDir confDir = do
  confs <- mapM (\path -> readConfFileIO (uConfDir </> path) >>= return. (path,) ) =<<
           filter (\f -> ".conf" `isSuffixOf` f &&
                         not (".hbrew.conf" `isSuffixOf` f)
                  ) `fmap` getDirectoryContents uConfDir
  let hbrews = filter (\(_,a) -> case importDirs a of
                          [] -> False
                          d:_ -> d `isSubDirectoryOf` libDir
                      ) confs
  files <- mapM (\(f,_) -> do
                    let conf = confDir </> f
                    e <- doesFileExist conf
                    if e
                      then throwIO $
                           mkIOError alreadyExistsErrorType "conf File" Nothing (Just conf)
                      else renameFile (uConfDir </> f) conf >> return conf
                ) hbrews
  pushFiles uConfDir files


isSubDirectoryOf :: FilePath -> FilePath -> Bool
isSubDirectoryOf _a _b = if isAbsolute _a && isAbsolute _b
                         then sub (splitDirectories _a) (splitDirectories _b)
                         else False
  where sub _ [] = True
        sub [] _ = False
        sub (a:as) (b:bs) = if a == b then sub as bs else False

reset :: FilePath -> IO ()
reset pDir = do
  hbrewConfs <- filter (".hbrew.conf" `isSuffixOf`) `fmap` getDirectoryContents pDir
  mapM_ (unlink. (pDir </>)) hbrewConfs
  recache

