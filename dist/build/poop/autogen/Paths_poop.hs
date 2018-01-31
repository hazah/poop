{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_poop (
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

bindir     = "/home/zhenya/.cabal/bin"
libdir     = "/home/zhenya/.cabal/lib/x86_64-linux-ghc-7.10.3/poop-0.1.0.0-1GScZmGSaRu98KtuoJZ5x0-poop"
dynlibdir  = "/home/zhenya/.cabal/lib/x86_64-linux-ghc-7.10.3"
datadir    = "/home/zhenya/.cabal/share/x86_64-linux-ghc-7.10.3/poop-0.1.0.0"
libexecdir = "/home/zhenya/.cabal/libexec/x86_64-linux-ghc-7.10.3/poop-0.1.0.0"
sysconfdir = "/home/zhenya/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "poop_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "poop_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "poop_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "poop_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "poop_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "poop_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
