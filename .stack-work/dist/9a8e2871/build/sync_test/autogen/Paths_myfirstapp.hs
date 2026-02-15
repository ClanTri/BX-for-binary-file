{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_myfirstapp (
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

bindir     = "E:\\haskell_test\\myfirstapp\\.stack-work\\install\\3db38ec7\\bin"
libdir     = "E:\\haskell_test\\myfirstapp\\.stack-work\\install\\3db38ec7\\lib\\x86_64-windows-ghc-8.2.2\\myfirstapp-0.1.0.0-a7NIQvsWTA6WQjXJrtrdd-sync_test"
dynlibdir  = "E:\\haskell_test\\myfirstapp\\.stack-work\\install\\3db38ec7\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "E:\\haskell_test\\myfirstapp\\.stack-work\\install\\3db38ec7\\share\\x86_64-windows-ghc-8.2.2\\myfirstapp-0.1.0.0"
libexecdir = "E:\\haskell_test\\myfirstapp\\.stack-work\\install\\3db38ec7\\libexec\\x86_64-windows-ghc-8.2.2\\myfirstapp-0.1.0.0"
sysconfdir = "E:\\haskell_test\\myfirstapp\\.stack-work\\install\\3db38ec7\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "myfirstapp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "myfirstapp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "myfirstapp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "myfirstapp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "myfirstapp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "myfirstapp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
