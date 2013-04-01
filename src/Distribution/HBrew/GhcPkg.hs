{-#LANGUAGE DeriveDataTypeable #-}
module Distribution.HBrew.GhcPkg(packageDir, PackageDirectoryTarget(..), recache) where

import Distribution.HBrew.Utils

import Control.Applicative
import Control.Monad(void)
import Control.Exception(throwIO)

import System.IO(hIsEOF, hClose, hGetLine)
import System.Process (CreateProcess(..), StdStream(..), proc)

recache :: String -> IO ()
recache ghcPkg = void $ createAndWaitProcess return (proc ghcPkg ["--user", "recache"])

data PackageDirectoryTarget = User | Global


packageDir :: String -> PackageDirectoryTarget -> IO FilePath
packageDir ghcPkg target = sub True
  where sub retry = createAndWaitProcess fun
                    (proc ghcPkg [targetToString target, "list"]){std_out = CreatePipe}

          where fun (_, Just stdout, _) = do
                  eof <- hIsEOF stdout
                  case (retry, eof) of
                    (True,  True) -> hClose stdout >> recache ghcPkg >> sub False
                    (False, True) -> throwIO $ userError "package recache error."
                    _             -> do dir <- init <$> hGetLine stdout
                                        hClose stdout >> return dir
                fun _ = error "packages': CreatePipe failed."

targetToString :: PackageDirectoryTarget -> String
targetToString User   = "--user"
targetToString Global = "--global"