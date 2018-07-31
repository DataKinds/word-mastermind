{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_word_mastermind (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tyler/.cabal/bin"
libdir     = "/home/tyler/.cabal/lib/x86_64-linux-ghc-8.0.2/word-mastermind-0.1.0.0-CkzWU5ICk1JK46IFDyI1Xr"
dynlibdir  = "/home/tyler/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/tyler/.cabal/share/x86_64-linux-ghc-8.0.2/word-mastermind-0.1.0.0"
libexecdir = "/home/tyler/.cabal/libexec"
sysconfdir = "/home/tyler/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "word_mastermind_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "word_mastermind_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "word_mastermind_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "word_mastermind_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "word_mastermind_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "word_mastermind_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
