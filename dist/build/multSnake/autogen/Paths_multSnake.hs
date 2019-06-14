{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_multSnake (
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

bindir     = "C:\\Users\\Lucas Aurelio\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Lucas Aurelio\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3\\multSnake-0.1.0.0-75Uzz9Pibz0CxFSzoPgUuU"
dynlibdir  = "C:\\Users\\Lucas Aurelio\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3"
datadir    = "C:\\Users\\Lucas Aurelio\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3\\multSnake-0.1.0.0"
libexecdir = "C:\\Users\\Lucas Aurelio\\AppData\\Roaming\\cabal\\multSnake-0.1.0.0-75Uzz9Pibz0CxFSzoPgUuU\\x86_64-windows-ghc-8.6.3\\multSnake-0.1.0.0"
sysconfdir = "C:\\Users\\Lucas Aurelio\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "multSnake_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "multSnake_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "multSnake_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "multSnake_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "multSnake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "multSnake_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
