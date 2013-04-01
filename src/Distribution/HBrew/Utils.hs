{-#LANGUAGE DeriveDataTypeable #-}
module Distribution.HBrew.Utils
       ( split
       , showText, readText
       , createAndWaitProcess
       , createDirectoryRecursive
       , getContentsRecursive
       , readConfFileIO
       ) where

import Control.Monad
import Control.Exception(Exception, throwIO)

import System.Directory
import System.FilePath
import System.Exit(ExitCode(..))
import System.IO(Handle)
import System.Process (CreateProcess(..), CmdSpec(..), createProcess, waitForProcess)

import Distribution.Text(Text, disp, simpleParse)
import Distribution.InstalledPackageInfo

import Text.PrettyPrint(render)

import Data.Typeable(Typeable)
import Data.Word


split :: (a -> Bool) -> [a] -> [[a]]
split p str = case span p str of
  (a,[]) -> [a]
  (a, b) -> a : split p (tail b)

readText :: Text a => String -> a 
readText = maybe (error "no parse") id . simpleParse

showText :: Text a => a -> String
showText = render. disp

data ExitFailureException = FailureShellCommand String Int
                          | FailureRawCommand   String [String] Int
                          deriving (Typeable)

instance Show ExitFailureException where
  show (FailureShellCommand cmd      code) = cmd ++ ": ExitFailure " ++ show code
  show (FailureRawCommand   cmd args code) =
    cmd ++ ' ': foldr (\i b -> i ++ ' ': b) "" args ++ ": ExitFailure " ++ show code

instance Exception ExitFailureException

type Handles = (Maybe Handle, Maybe Handle, Maybe Handle)

createAndWaitProcess :: (Handles -> IO a) -> CreateProcess -> IO a
createAndWaitProcess f cp = do
  (i,o,e,h) <- createProcess cp
  ret <- f (i,o,e)
  waitForProcess h >>= \code -> case code of
    ExitSuccess    -> return ret
    ExitFailure ec -> throwIO $ exception ec
    where exception = case cmdspec cp of
            ShellCommand cmd    -> FailureShellCommand cmd
            RawCommand cmd args -> FailureRawCommand cmd args

createDirectoryRecursive :: FilePath -> [FilePath] -> IO FilePath
createDirectoryRecursive base [] = return base
createDirectoryRecursive base (p:ps) = do
  let d = base </> p
  e <- doesDirectoryExist d
  when (not e) $ createDirectory d
  createDirectoryRecursive d ps

getContentsRecursive :: Word -> FilePath -> IO [FilePath]
getContentsRecursive ilim dir = sub ilim ""
  where sub 0   _   = return []
        sub lim rel = do
          cont  <- filter (`notElem` [".", ".."]) `fmap` getDirectoryContents (dir </> rel)
          files <- filterM (doesFileExist     . (dir </>)) $ map (rel </>) cont
          dirs  <- filterM (doesDirectoryExist. (dir </>)) $ map (rel </>) cont
          subc  <- mapM (sub (pred lim)) dirs
          return $ files ++ concat subc


newtype PErrorException = PErrorException PError deriving (Show, Typeable)
instance Exception PErrorException

readConfFileIO :: FilePath -> IO InstalledPackageInfo
readConfFileIO path = parseInstalledPackageInfo `fmap` readFile path >>= \info -> case info of
  ParseFailed perr -> throwIO $ PErrorException perr
  ParseOk _   i    -> return i
