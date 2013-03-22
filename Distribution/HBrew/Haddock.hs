module Distribution.HBrew.Haddock (genIndex) where

import Control.Applicative
import System.Process

import Distribution.Package
import Distribution.HBrew.Utils
import Distribution.HBrew.DepGraph
import Distribution.InstalledPackageInfo

import Data.Maybe
import Data.List

haddock :: [String] -> CreateProcess
haddock = proc "haddock"

type ReadInterface = (FilePath, FilePath)

readInterfaces :: Graph -> [ReadInterface]
readInterfaces gr = catMaybes. map sub. uniquify. filter exposed. map packageInfo $ flatten gr
  where sub pinfo = (,) <$> listToMaybe (haddockHTMLs      pinfo)
                    <*> listToMaybe (haddockInterfaces pinfo)

uniquify :: [InstalledPackageInfo] -> [InstalledPackageInfo]
uniquify []     = []
uniquify (ipi:ipis) =
  let (eq, o) = partition ((== pName ipi). pName) ipis
  in maximumBy (\a b -> pVer a `compare` pVer b) (ipi:eq) : uniquify o
  where pName = packageName    . sourcePackageId
        pVer  = packageVersion . sourcePackageId


genIndex :: FilePath -> Graph -> IO ()
genIndex odir graph = do
  createAndWaitProcess (const $ return ()) (haddock args)
  where sub (html, ifs) = "--read-interface=" ++ html ++ ',' : ifs
        args = [ "--odir=" ++ odir
               , "--gen-contents"
               , "--gen-index"
               , "--title=Haskell modules on this system"
               ] ++ map sub (readInterfaces graph)
