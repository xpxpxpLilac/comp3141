{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Ex05 (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Lilac Liu\\Desktop\\comp3141\\week7\\Ex05\\.stack-work\\install\\6842c354\\bin"
libdir     = "C:\\Users\\Lilac Liu\\Desktop\\comp3141\\week7\\Ex05\\.stack-work\\install\\6842c354\\lib\\x86_64-windows-ghc-8.2.2\\Ex05-1.0-BKQfgR9BrVDeol6sJDuKe-Ex05"
dynlibdir  = "C:\\Users\\Lilac Liu\\Desktop\\comp3141\\week7\\Ex05\\.stack-work\\install\\6842c354\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "C:\\Users\\Lilac Liu\\Desktop\\comp3141\\week7\\Ex05\\.stack-work\\install\\6842c354\\share\\x86_64-windows-ghc-8.2.2\\Ex05-1.0"
libexecdir = "C:\\Users\\Lilac Liu\\Desktop\\comp3141\\week7\\Ex05\\.stack-work\\install\\6842c354\\libexec\\x86_64-windows-ghc-8.2.2\\Ex05-1.0"
sysconfdir = "C:\\Users\\Lilac Liu\\Desktop\\comp3141\\week7\\Ex05\\.stack-work\\install\\6842c354\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ex05_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ex05_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Ex05_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Ex05_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ex05_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ex05_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
