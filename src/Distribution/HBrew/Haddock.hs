module Distribution.HBrew.Haddock (genIndex) where

import Control.Applicative
import Control.Monad
import System.Process
import System.Directory

import Distribution.Package
import Distribution.HBrew.Utils
import Distribution.HBrew.DepGraph
import Distribution.InstalledPackageInfo

import Data.Maybe
import Data.List
import Data.Function

type ReadInterface = (FilePath, FilePath)

readInterfaces :: Graph -> IO [ReadInterface]
readInterfaces gr = filterM exists. mapMaybe toReadInterface.
                    uniquify. filter exposed. map packageInfo $ flatten gr
  where toReadInterface pinfo = (,) <$>
                                listToMaybe (haddockHTMLs      pinfo) <*>
                                listToMaybe (haddockInterfaces pinfo)
        exists (_, hdc) = doesFileExist hdc
                          >>= \e -> if e
                                    then return True
                                    else do putStr "Warning: "
                                            putStr $ show hdc
                                            putStrLn " does not exists."
                                            return False

uniquify :: [InstalledPackageInfo] -> [InstalledPackageInfo]
uniquify []     = []
uniquify (ipi:ipis) =
  let (eq, o) = partition ((== pName ipi). pName) ipis
  in maximumBy (compare `on` pVer) (ipi:eq) : uniquify o
  where pName = packageName    . sourcePackageId
        pVer  = packageVersion . sourcePackageId


genIndex :: String -> FilePath -> Graph -> IO ()
genIndex haddock odir graph = do
  ri <- readInterfaces graph
  createAndWaitProcess (const $ return ()) (proc haddock $ args ++ map sub ri)
  where sub (html, ifs) = "--read-interface=" ++ html ++ ',' : ifs
        args = [ "--odir=" ++ odir
               , "--gen-contents"
               , "--gen-index"
               , "--title=Haskell modules on this system"
               ]
