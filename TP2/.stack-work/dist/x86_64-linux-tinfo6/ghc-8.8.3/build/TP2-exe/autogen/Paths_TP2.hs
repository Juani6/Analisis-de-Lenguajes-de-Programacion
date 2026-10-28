{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP2 (
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

bindir     = "/home/juani/Documents/alp/TP2/.stack-work/install/x86_64-linux-tinfo6/eaac73a87820529a2576da9f88a26e1ef495432ef47c5dd7973b75de8970da60/8.8.3/bin"
libdir     = "/home/juani/Documents/alp/TP2/.stack-work/install/x86_64-linux-tinfo6/eaac73a87820529a2576da9f88a26e1ef495432ef47c5dd7973b75de8970da60/8.8.3/lib/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0-2c9qde79sPOLVEqyS9Uqj7-TP2-exe"
dynlibdir  = "/home/juani/Documents/alp/TP2/.stack-work/install/x86_64-linux-tinfo6/eaac73a87820529a2576da9f88a26e1ef495432ef47c5dd7973b75de8970da60/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/juani/Documents/alp/TP2/.stack-work/install/x86_64-linux-tinfo6/eaac73a87820529a2576da9f88a26e1ef495432ef47c5dd7973b75de8970da60/8.8.3/share/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
libexecdir = "/home/juani/Documents/alp/TP2/.stack-work/install/x86_64-linux-tinfo6/eaac73a87820529a2576da9f88a26e1ef495432ef47c5dd7973b75de8970da60/8.8.3/libexec/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
sysconfdir = "/home/juani/Documents/alp/TP2/.stack-work/install/x86_64-linux-tinfo6/eaac73a87820529a2576da9f88a26e1ef495432ef47c5dd7973b75de8970da60/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
