module Distribution.HBrew.Compatibility(link, unlink, shortcut, unShortcut) where

import System.Posix.Files

link :: FilePath -> FilePath -> IO ()
link = createSymbolicLink

unlink :: FilePath -> IO ()
unlink = removeLink

shortcut :: FilePath -> FilePath -> IO ()
shortcut = createSymbolicLink

unShortcut :: FilePath -> IO ()
unShortcut = removeLink
