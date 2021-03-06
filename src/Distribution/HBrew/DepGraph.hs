{-#LANGUAGE ViewPatterns, NamedFieldPuns#-}
module Distribution.HBrew.DepGraph
       ( Graph, isUserPkg
       , Node, packageInfo, cabalFile
       , flatten
       , lookupNodesFuzzy
       , ancestor
       , makeConfigGraph, makeProcedure
       ) where

import System.FilePath

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.IntMap as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Array
import Data.Maybe
import Data.List

import Distribution.Package hiding (depends)
import Distribution.Version
import Distribution.InstalledPackageInfo
import Distribution.HBrew.Utils

data Graph = Graph { nodes    :: Map Node Int
                   , edges    :: Array (Int,Int) Bool
                   , dict     :: Map InstalledPackageId PackageId
                   , revDict  :: Map PackageId [InstalledPackageId]
                   }
           deriving Show

flatten :: Graph -> [Node]
flatten Graph{nodes} = M.keys nodes

updateDict :: Graph -> Graph
updateDict gr@Graph{nodes} =
  let (dict, revDict) =
        M.foldlWithKey (\(d, r) n _ -> let iid = installedPackageId $ packageInfo n
                                           pid = sourcePackageId $ packageInfo n
                                       in (M.insert iid pid d, M.insertWith (++) pid [iid] r)
                       ) (M.empty, M.empty) nodes
  in gr{dict = dict, revDict = revDict}

hasEdgeIdx :: Graph -> Int -> Int -> Bool
hasEdgeIdx Graph{edges} a b = edges ! (a,b)

data Node = UserPkg   { packageInfo :: InstalledPackageInfo
                      , cabalFile   :: FilePath
                      }
          | GlobalPkg { packageInfo :: InstalledPackageInfo
                      , cabalFile   :: FilePath
                      }
          deriving Show

isUserPkg :: Node -> Bool
isUserPkg UserPkg{} = True
isUserPkg _         = False

isGlobalPkg :: Node -> Bool
isGlobalPkg GlobalPkg{} = True
isGlobalPkg _           = False


instance Eq Node where
  a == b = installedPackageId (packageInfo a) == installedPackageId (packageInfo b)

instance Ord Node where
  (packageInfo -> a) `compare` (packageInfo -> b) =
    case sourcePackageId a `compare` sourcePackageId b
    of EQ -> installedPackageId a `compare` installedPackageId b
       o  -> o

makeConfigGraph :: [FilePath] -> [FilePath] -> IO Graph
makeConfigGraph uConfs gConfs = do
  hInfo <- mapM (\f -> readConfFileIO f >>= \info -> return $ UserPkg info f) $
           filter ((== ".conf") . takeExtension) uConfs
  gInfo <- mapM (\f -> readConfFileIO f >>= \info -> return $ GlobalPkg info f) gConfs

  let info     = gInfo ++ hInfo
      nodes    = M.fromList $ zip info [0..]
      revNodes = IM.fromList $ zip [0..] info
      rel      = map (\i -> let inf = packageInfo i
                            in (installedPackageId inf, sourcePackageId inf)
                     ) info
      dict     = M.fromList rel
      revDict  = M.fromListWith (++) $ map (\(i,p) -> (p, [i])) rel
      size  = M.size nodes - 1
      edges = [((x,y), has) | x <- [0 .. size], y <- [0 .. size]
                            , let has = case  IM.lookup x revNodes of
                                    Nothing -> False
                                    Just nx -> let iids = depends $ packageInfo nx
                                                   nds  = map (\i -> lookupNode' i dict nodes) iids
                                                   ys   = mapMaybe (\i -> case i of
                                                                       Just ny -> M.lookup ny nodes
                                                                       Nothing -> Nothing
                                                                   ) nds
                                               in y `elem` ys
                            ]
  return $ Graph nodes (array ((0,0), (size,size)) edges) dict revDict


type Procedure = (Graph, [PackageId])

makeProcedure :: Graph -> [PackageId] -> Procedure
makeProcedure _all pids =
  let nodes = nub $ concatMap (`lookupNodesByPid` _all) pids
      rej    = rejectConflicts pids . rejectConflictsWithToInstalls pids $ descendant _all nodes
      ins    = filter (`notMemberPid` rej) pids
  in (dropGlobal rej, ins)

rejectConflictsWithToInstalls :: [PackageId] -> Graph -> Graph
rejectConflictsWithToInstalls _ins _gr@Graph{nodes = _nodes} =
  let torej = ancestorIdx _gr. IS.fromList. M.elems $ M.filterWithKey
              (\k _ -> let spid = (sourcePackageId. packageInfo) k
                       in foldl' (\b i -> ( packageName spid == packageName i &&
                                            packageVersion spid /= packageVersion i) || b
                                 ) False _ins
              ) _nodes
  in updateDict $ _gr{ nodes = M.filter (`IS.notMember` torej) _nodes }

rejectConflicts :: [PackageId] -> Graph -> Graph
rejectConflicts _ins _gr = updateDict $ _gr{nodes = foldl' sub (nodes _gr) $ conflicts _gr}
  where sub nds c =
          let hold = IS.fromList. M.elems $
                     case M.filterWithKey (\k _ -> (sourcePackageId. packageInfo) k `elem` _ins) c of
                       m | M.null m  -> M.filterWithKey (\k _ -> isGlobalPkg k) c
                         | otherwise -> m
              all_  = IS.fromList $ M.elems c
              torej = ancestorIdx _gr $ all_ `IS.difference` hold
          in M.filter (`IS.notMember` torej) nds

conflicts :: Graph -> [Map Node Int]
conflicts Graph{nodes} = sub nodes
  where sub m = case M.minViewWithKey m of
          Nothing -> []
          Just ((n,i), mp) ->
            let name   = packageName. sourcePackageId . packageInfo
                (c, o) = M.partitionWithKey (\k _ -> name n == name k) mp
            in if M.null c then sub o else M.insert n i c : sub o

ancestorIdx :: Graph -> IntSet -> IntSet
ancestorIdx = dfsIdx parentIdx

ancestor :: Graph -> [Node] -> Graph
ancestor = (updateDict.) . dfs parentIdx

descendant :: Graph -> [Node] -> Graph
descendant = (updateDict.) . dfs childrenIdx

childrenIdx, parentIdx :: Graph -> Int -> IntSet
childrenIdx gr@Graph{nodes} i = IS.filter (hasEdgeIdx gr i) . IS.fromList $ M.elems nodes
parentIdx   gr@Graph{nodes} i = IS.filter (flip (hasEdgeIdx gr) i) . IS.fromList $ M.elems nodes

dfs :: (Graph -> Int -> IntSet) -> Graph -> [Node] -> Graph
dfs fun gr@Graph{nodes} nds =
  let idx = IS.fromList . catMaybes $ map (`M.lookup` nodes) nds
      set = dfsIdx fun gr idx
  in gr{nodes = M.filter (`IS.member` set) nodes}

dfsIdx :: (Graph -> Int -> IntSet) -> Graph -> IntSet -> IntSet
dfsIdx fun gr _is = _is `IS.union` go IS.empty _is
  where go done is
          | IS.null is = IS.empty
          | otherwise  = let neighbor = IS.foldl (\a k -> fun gr k `IS.union` a) IS.empty is
                             next     = IS.filter (`IS.notMember` done) neighbor
                             nextdone = done `IS.union` neighbor
                         in neighbor `IS.union` go nextdone next

lookupNodesFuzzy :: PackageId -> Graph -> [Node]
lookupNodesFuzzy pid gr = 
  if packageVersion pid == Version [] []
    then lookupNodesByPName (packageName pid) gr
    else lookupNodesByPid   pid gr

lookupNodesByPName :: PackageName -> Graph -> [Node]
lookupNodesByPName pname Graph{revDict, dict, nodes} = do
  iids <- M.elems $ M.filterWithKey (curry $ (== pname). packageName .fst) revDict
  mapMaybe (\iid -> lookupNode' iid dict nodes) iids


lookupNodesByPid :: PackageId -> Graph -> [Node]
lookupNodesByPid pid Graph{revDict, dict, nodes} = do
  iids <- maybeToList $ M.lookup pid revDict
  mapMaybe (\iid -> lookupNode' iid dict nodes) iids

lookupNode' :: InstalledPackageId -> Map InstalledPackageId PackageId -> Map Node b -> Maybe Node
lookupNode' iid dict nodes = do pid <- M.lookup iid dict
                                idx <- M.lookupIndex (UserPkg emptyInstalledPackageInfo {
                                                         installedPackageId = iid,
                                                         sourcePackageId    = pid
                                                         } "") nodes
                                return . fst $ M.elemAt idx nodes

notMemberPid :: PackageId -> Graph -> Bool
notMemberPid pid Graph{revDict} = pid `M.notMember` revDict

dropGlobal :: Graph -> Graph
dropGlobal gr@Graph{nodes} =
  updateDict $ gr{nodes = M.filterWithKey (\k _ -> isUserPkg k) nodes}
