{-#LANGUAGE DeriveDataTypeable, ViewPatterns, NoMonomorphismRestriction, NamedFieldPuns #-}

module Distribution.HBrew.DepGraph
       ( Node(packageInfo, cabalFile)
       , SubGraph(root)
       , Graph
       , Graphs
       , makeConfigGraph
       , flatten, isUserPkg
       , overlap, isOverlap
       , lookupSubGraph
       , rootPkgId
       ) where

import Control.Exception
import System.FilePath
import Control.Monad
import System.Directory
import Distribution.Package (PackageId, InstalledPackageId, packageName, packageVersion)
import Distribution.InstalledPackageInfo
import Data.List
import Data.Typeable(Typeable)
import Data.Word(Word)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Distribution.HBrew.GhcPkg

newtype PErrorException = PErrorException PError deriving (Show, Typeable)
instance Exception PErrorException

data Node = UserPkg { packageInfo :: InstalledPackageInfo
                    , cabalFile   :: FilePath
                    }
          | GlobalPkg { packageInfo :: InstalledPackageInfo
                      , cabalFile   :: FilePath
                      }
          deriving Show

isUserPkg :: Node -> Bool
isUserPkg UserPkg{} = True
isUserPkg _ = False

dummyNode :: InstalledPackageId -> PackageId -> Node
dummyNode ipid spid =
  UserPkg { packageInfo = emptyInstalledPackageInfo { installedPackageId = ipid
                                                    , sourcePackageId    = spid
                                                    }
          , cabalFile   = ""
          }

instance Eq Node where
  a == b = installedPackageId (packageInfo a) == installedPackageId (packageInfo b)

instance Ord Node where
  (packageInfo -> a) `compare` (packageInfo -> b) =
    case sourcePackageId a `compare` sourcePackageId b
    of EQ -> installedPackageId a `compare` installedPackageId b
       o  -> o

data Graph = Graph { edges   :: Map Node [InstalledPackageId]
                   , dict    :: Map InstalledPackageId PackageId
                   , revDict :: Map PackageId InstalledPackageId
                   }
           deriving Show

data SubGraph = SubGraph { graph :: Graph
                         , root  :: InstalledPackageId
                         }
              deriving Show

class Graphs a where
  getGraph :: a -> Graph

instance Graphs Graph where
  getGraph = id

instance Graphs SubGraph where
  getGraph = graph

instance Graphs a => Graphs (b, a) where
  getGraph = getGraph. snd

readConfFileIO :: FilePath -> IO InstalledPackageInfo
readConfFileIO path = parseInstalledPackageInfo `fmap` readFile path >>= \info -> case info of
  ParseFailed perr -> throwIO $ PErrorException perr
  ParseOk _   i    -> return i

getContentsRecursive :: Word -> FilePath -> IO [FilePath]
getContentsRecursive ilim dir = sub ilim ""
  where sub 0   _   = return []
        sub lim rel = do
          cont  <- filter (`notElem` [".", ".."]) `fmap` getDirectoryContents (dir </> rel)
          files <- filterM (doesFileExist     . (dir </>)) $ map (rel </>) cont
          dirs  <- filterM (doesDirectoryExist. (dir </>)) $ map (rel </>) cont
          subc  <- mapM (sub (pred lim)) dirs
          return $ files ++ concat subc

makeConfigGraph :: FilePath -> Word -> IO Graph
makeConfigGraph path limit = do
  hbrew  <- filter (".conf" `isSuffixOf`) `fmap` getContentsRecursive limit path
  hInfo  <- mapM (\f -> readConfFileIO ( path </> f) >>= \info -> return $ UserPkg   info f) hbrew

  gPath  <- packageDir Global
  global <- filter (".conf" `isSuffixOf`) `fmap` getDirectoryContents gPath
  gInfo  <- mapM (\f -> readConfFileIO (gPath </> f) >>= \info -> return $ GlobalPkg info f) global

  let info = gInfo ++ hInfo
      nodes = M.fromList $ map (\i -> (i, depends $ packageInfo i)) info
      tdict = map (\i -> let ifo = packageInfo i
                         in (installedPackageId ifo, sourcePackageId ifo)) info
  return $ Graph nodes (M.fromList tdict) (M.fromList $ map swap tdict)

swap :: (t1, t) -> (t, t1)
swap (a,b) = (b,a)

subGraph :: Graphs a => a -> InstalledPackageId -> SubGraph
subGraph (getGraph -> gr@Graph{edges, dict, revDict}) top_ =
  (\e -> SubGraph (Graph e dict revDict) top_). M.fromList $ sub top_
  where sub top = case M.lookup top dict >>= \pid -> M.lookup (dummyNode top pid) edges of
          Nothing   -> []
          Just pkgs -> let node = maybe (error $ "subGraph: lookup error.") id $ getNode gr top
                       in (node, pkgs): concat (map sub pkgs)

lookupSubGraph :: Graphs a => a -> PackageId -> Maybe SubGraph
lookupSubGraph (getGraph -> gr@Graph{revDict = rd}) top =
  subGraph gr `fmap` M.lookup top rd

flatten :: Graphs a => a -> [Node]
flatten (getGraph -> Graph{edges = e}) = M.keys e

getNode :: Graphs a => a -> InstalledPackageId -> Maybe Node
getNode (getGraph -> Graph{edges, dict} ) top = do
  idx <- M.lookup top dict >>= \pid -> M.lookupIndex (dummyNode top pid) edges
  return. fst $ M.elemAt idx edges

overlap :: Graphs a => a -> a -> Maybe (a, a)
overlap ga gb = case
  intersectBy' (\a b -> packageName    a == packageName    b &&
                        packageVersion a /= packageVersion b
               ) aPids bPids of
    []      -> Nothing
    (a,b):_ -> Just $ if a > b then (ga, gb) else (gb, ga)
  where aPids = map (sourcePackageId. packageInfo) (flatten $ getGraph ga)
        bPids = map (sourcePackageId. packageInfo) (flatten $ getGraph gb)

isOverlap :: Graphs a => a -> a -> Bool
isOverlap a b = isJust $ overlap a b

rootPkgId :: SubGraph ->  PackageId
rootPkgId SubGraph{graph = Graph{dict}, root} =
  maybe (error "rootPkgId: PackageId not found.") id $ M.lookup root dict


intersectBy'             :: (a -> a -> Bool) -> [a] -> [a] -> [(a, a)]
intersectBy' _  [] _     =  []
intersectBy' _  _  []    =  []
intersectBy' eq xs ys    =  [(x,fromJust y) | x <- xs, let y = find (eq x) ys, isJust y]