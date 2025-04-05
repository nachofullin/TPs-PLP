{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tp (
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

bindir     = "/home/titan/.cabal/bin"
libdir     = "/home/titan/.cabal/lib/x86_64-linux-ghc-8.8.4/tp-0.1.0.0-inplace-tp"
dynlibdir  = "/home/titan/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/titan/.cabal/share/x86_64-linux-ghc-8.8.4/tp-0.1.0.0"
libexecdir = "/home/titan/.cabal/libexec/x86_64-linux-ghc-8.8.4/tp-0.1.0.0"
sysconfdir = "/home/titan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
