module Distribution.HBrew.Compatibility(link, unlink) where

import System.Posix.Files

link :: FilePath -> FilePath -> IO ()
link = createSymbolicLink

unlink :: FilePath -> IO ()
unlink = removeLink