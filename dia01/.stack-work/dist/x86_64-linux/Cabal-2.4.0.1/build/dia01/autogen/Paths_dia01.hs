{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_dia01 (
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

bindir     = "/home/ufabc/Curso-Haskell/dia01/.stack-work/install/x86_64-linux/5d20e13f2e39bf505eace456d632478d7c4c173682a2646bb81ac2ebaeb45c74/8.6.4/bin"
libdir     = "/home/ufabc/Curso-Haskell/dia01/.stack-work/install/x86_64-linux/5d20e13f2e39bf505eace456d632478d7c4c173682a2646bb81ac2ebaeb45c74/8.6.4/lib/x86_64-linux-ghc-8.6.4/dia01-0.1.0.0-KejPe23aPfB6aLAzb6l4na-dia01"
dynlibdir  = "/home/ufabc/Curso-Haskell/dia01/.stack-work/install/x86_64-linux/5d20e13f2e39bf505eace456d632478d7c4c173682a2646bb81ac2ebaeb45c74/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/ufabc/Curso-Haskell/dia01/.stack-work/install/x86_64-linux/5d20e13f2e39bf505eace456d632478d7c4c173682a2646bb81ac2ebaeb45c74/8.6.4/share/x86_64-linux-ghc-8.6.4/dia01-0.1.0.0"
libexecdir = "/home/ufabc/Curso-Haskell/dia01/.stack-work/install/x86_64-linux/5d20e13f2e39bf505eace456d632478d7c4c173682a2646bb81ac2ebaeb45c74/8.6.4/libexec/x86_64-linux-ghc-8.6.4/dia01-0.1.0.0"
sysconfdir = "/home/ufabc/Curso-Haskell/dia01/.stack-work/install/x86_64-linux/5d20e13f2e39bf505eace456d632478d7c4c173682a2646bb81ac2ebaeb45c74/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dia01_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dia01_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "dia01_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "dia01_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dia01_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dia01_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
