
module Distribution.HBrew.Ghc where

import System.IO(hGetLine)
import System.Process (CreateProcess(..), StdStream(..), proc)

import Distribution.System
import Distribution.Simple.Compiler
import Distribution.HBrew.Utils
import Distribution.Version

ghcVersion :: String -> IO (CompilerFlavor, Version, Arch)
ghcVersion ghc = do
  let comp = maybe buildCompilerFlavor id defaultCompilerFlavor
  let arch = buildArch
  v <- readText `fmap`
       createAndWaitProcess fun (proc ghc ["--numeric-version"]){std_out = CreatePipe}
  return (comp, v :: Version, arch)
  where fun (_, Just stdout, _) = hGetLine stdout
        fun _                   = error "ghcVersion: CreatePipe failed."

showGhcVersion :: (CompilerFlavor, Version, Arch) -> String
showGhcVersion (f, v, a) = showText f ++ '-': showText v ++ '-': showText a
