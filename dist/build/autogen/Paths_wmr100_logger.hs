module Paths_wmr100_logger (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/gael/.cabal/bin"
libdir     = "/Users/gael/.cabal/lib/wmr100-logger-0.1.0.0/ghc-7.4.2"
datadir    = "/Users/gael/.cabal/share/wmr100-logger-0.1.0.0"
libexecdir = "/Users/gael/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wmr100_logger_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wmr100_logger_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wmr100_logger_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wmr100_logger_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
