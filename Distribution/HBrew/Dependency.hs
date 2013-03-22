{-#LANGUAGE NoMonomorphismRestriction#-}
module Distribution.HBrew.Dependency
       (Procedure
       , makeProcedure
       ) where

import Distribution.Package
import Distribution.InstalledPackageInfo
import Distribution.HBrew.DepGraph
import Data.List
import Debug.Trace

type Procedure = ([SubGraph], [PackageId])

isConflictWithToInstall :: [PackageId] -> SubGraph -> Bool
isConflictWithToInstall toInstall subGraph = do
  any (\a -> let spid = sourcePackageId $ packageInfo a
             in traceShow spid
                foldl' (\b i -> ( packageName    spid == packageName    i  &&
                                  packageVersion spid /= packageVersion i) || b
                       ) False toInstall
      ) $ flatten subGraph

conflictWithToPush :: [SubGraph] -> SubGraph -> ([SubGraph], Maybe PackageId)
conflictWithToPush toPush subGraph =
  case foldl sub ([], Nothing) toPush of
    (psh, Nothing) -> (subGraph:psh, Nothing)
    (psh, Just o)  -> (psh, Just o)
  where sub (psh, Just a)  b = (b:psh, Just a)
        sub (psh, Nothing) b = case overlap subGraph b of
          Nothing         -> (b:psh, Nothing)
          Just (gtr, lsr) -> (gtr:psh, Just $ rootPkgId lsr)

makeProcedure :: Graph -> [PackageId] -> Procedure
makeProcedure grh toInstall =
  let (psh, ins) = foldr (\a (p,i) -> case lookupSubGraph grh a of
                             Just gr -> if isConflictWithToInstall toInstall gr
                                        then (p, a:i)
                                        else (gr:p, i)
                             Nothing -> (p, a:i)
{-
                               if traceShow gr isConflictWithToInstall toInstall gr
                                        then (p, a:i)
                                        else (gr:p, i) - case conflictWithToPush p gr
                                             of (p', Nothing) -> (p', i)
                                                (p', Just i') -> (p', i':i)
                             Nothing -> (p, a:i) -}
                         ) ([], []) toInstall
  in (psh, ins)



