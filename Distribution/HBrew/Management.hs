module Distribution.HBrew.Management(push, reset) where

import Control.Monad

import System.FilePath
import System.Directory
import Distribution.HBrew.DepGraph
import Distribution.HBrew.Compatibility
import Distribution.HBrew.GhcPkg
import Data.List

push :: Graphs a => FilePath -> FilePath -> a -> IO ()
push hbrewDir pDir gr = do
  let files = map cabalFile. filter isUserPkg $ flatten gr
  mapM_ (\f -> do let target = pDir </> takeBaseName f <.> "hbrew.conf"
                  e <- doesFileExist target
                  when (not e) $ link (hbrewDir </> f) target
        ) files

reset :: FilePath -> IO ()
reset pDir = do
  hbrewConfs <- filter (".hbrew.conf" `isSuffixOf`) `fmap` getDirectoryContents pDir
  mapM_ (unlink. (pDir </>)) hbrewConfs
  recache

