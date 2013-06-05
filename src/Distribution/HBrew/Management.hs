{-#LANGUAGE TupleSections#-}
module Distribution.HBrew.Management(push, pull, reset, programs) where

import Control.Monad

import System.IO
import System.FilePath
import System.Directory
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Compatibility
import Distribution.HBrew.Utils
import Distribution.HBrew.GhcPkg
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Version

import Data.List

push :: FilePath -> FilePath -> [Node] -> IO ()
push hbrewDir pDir nodes =
  pushFiles pDir (map ((hbrewDir </>). cabalFile) $ filter isUserPkg nodes)

pushFiles :: FilePath -> [FilePath] -> IO ()
pushFiles ucDir =
  mapM_ $ \f -> do let target = ucDir </> takeBaseName f <.> "hbrew.conf"
                   e <- doesFileExist target
                   unless e $ link f target

pull :: FilePath -> FilePath -> FilePath -> IO ()
pull uConfDir libDir confDir = do
  confs <- mapM (\path -> (path,) `fmap` readConfFileIO (uConfDir </> path)) =<<
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
                    when e $ do
                      renameFile conf (conf <.> "bak")
                      hPutStrLn stderr $
                        "Warning: " ++ conf ++ " file already exist. move to " ++ conf <.> "bak."
                    renameFile (uConfDir </> f) conf >> return conf
                ) hbrews
  pushFiles uConfDir files


isSubDirectoryOf :: FilePath -> FilePath -> Bool
isSubDirectoryOf _a _b =
  (isAbsolute _a && isAbsolute _b) && sub (splitDirectories _a) (splitDirectories _b)
  where sub _ [] = True
        sub [] _ = False
        sub (a:as) (b:bs) = (a == b) && sub as bs

reset :: String -> FilePath -> IO ()
reset ghcPkg pDir = do
  hbrewConfs <- filter (".hbrew.conf" `isSuffixOf`) `fmap` getDirectoryContents pDir
  mapM_ (unlink. (pDir </>)) hbrewConfs
  recache ghcPkg

programs :: FilePath -> IO [(PackageName, [(Version, [String])])]
programs dir = do
  pNames <- dropDots `fmap` getDirectoryContents dir
  vids   <- forM pNames $ \p -> do
    versions  <- dropDots `fmap` getDirectoryContents (dir </> p)
    binIds    <- forM versions $ \v -> do
      ids <- dropDots `fmap` getDirectoryContents (dir </> p </> v)
      e   <- forM ids $ \i -> doesDirectoryExist (dir </> p </> v </> i </> "bin")
      return .map fst. filter snd $ zip ids e
    return. filter (not . null. snd) $ zip (map readText versions) binIds
  return . filter (not. null. snd) $ zip (map readText pNames) vids
  where dropDots = filter (`notElem` [".", ".."])

