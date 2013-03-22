module Distribution.HBrew.Management(push, reset) where

import Control.Monad

import System.FilePath
import System.Directory
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Compatibility
import Distribution.HBrew.GhcPkg
import Data.List

push :: FilePath -> FilePath -> [Node] -> IO ()
push hbrewDir pDir nodes = do
  let files = map cabalFile $ filter isUserPkg nodes
  mapM_ (\f -> do let target = pDir </> takeBaseName f <.> "hbrew.conf"
                  e <- doesFileExist target
                  when (not e) $ link (hbrewDir </> f) target
        ) files

reset :: FilePath -> IO ()
reset pDir = do
  hbrewConfs <- filter (".hbrew.conf" `isSuffixOf`) `fmap` getDirectoryContents pDir
  mapM_ (unlink. (pDir </>)) hbrewConfs
  recache

