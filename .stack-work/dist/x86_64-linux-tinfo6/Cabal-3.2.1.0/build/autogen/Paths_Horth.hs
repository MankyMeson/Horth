{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Horth (
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

bindir     = "/home/doyle/Documents/HaskellProjects/Horth/.stack-work/install/x86_64-linux-tinfo6/49259395687695c837c5d1736fab60eaef8aef9ec94b5ab74faccb1e3e29b2a4/8.10.7/bin"
libdir     = "/home/doyle/Documents/HaskellProjects/Horth/.stack-work/install/x86_64-linux-tinfo6/49259395687695c837c5d1736fab60eaef8aef9ec94b5ab74faccb1e3e29b2a4/8.10.7/lib/x86_64-linux-ghc-8.10.7/Horth-0.1.0.0-K3wmDTrIvhc2m5xo4tHu9p"
dynlibdir  = "/home/doyle/Documents/HaskellProjects/Horth/.stack-work/install/x86_64-linux-tinfo6/49259395687695c837c5d1736fab60eaef8aef9ec94b5ab74faccb1e3e29b2a4/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/doyle/Documents/HaskellProjects/Horth/.stack-work/install/x86_64-linux-tinfo6/49259395687695c837c5d1736fab60eaef8aef9ec94b5ab74faccb1e3e29b2a4/8.10.7/share/x86_64-linux-ghc-8.10.7/Horth-0.1.0.0"
libexecdir = "/home/doyle/Documents/HaskellProjects/Horth/.stack-work/install/x86_64-linux-tinfo6/49259395687695c837c5d1736fab60eaef8aef9ec94b5ab74faccb1e3e29b2a4/8.10.7/libexec/x86_64-linux-ghc-8.10.7/Horth-0.1.0.0"
sysconfdir = "/home/doyle/Documents/HaskellProjects/Horth/.stack-work/install/x86_64-linux-tinfo6/49259395687695c837c5d1736fab60eaef8aef9ec94b5ab74faccb1e3e29b2a4/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Horth_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Horth_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Horth_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Horth_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Horth_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Horth_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
