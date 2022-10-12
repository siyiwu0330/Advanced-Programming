{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_letitbe (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Project\\Advanced-Programming\\Assignment5\\code\\letitbe\\.stack-work\\install\\341e2831\\bin"
libdir     = "D:\\Project\\Advanced-Programming\\Assignment5\\code\\letitbe\\.stack-work\\install\\341e2831\\lib\\x86_64-windows-ghc-9.0.2\\letitbe-0.0.0-5ONyAHcgmG4Es8DASH97f7"
dynlibdir  = "D:\\Project\\Advanced-Programming\\Assignment5\\code\\letitbe\\.stack-work\\install\\341e2831\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "D:\\Project\\Advanced-Programming\\Assignment5\\code\\letitbe\\.stack-work\\install\\341e2831\\share\\x86_64-windows-ghc-9.0.2\\letitbe-0.0.0"
libexecdir = "D:\\Project\\Advanced-Programming\\Assignment5\\code\\letitbe\\.stack-work\\install\\341e2831\\libexec\\x86_64-windows-ghc-9.0.2\\letitbe-0.0.0"
sysconfdir = "D:\\Project\\Advanced-Programming\\Assignment5\\code\\letitbe\\.stack-work\\install\\341e2831\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "letitbe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "letitbe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "letitbe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "letitbe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "letitbe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "letitbe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
