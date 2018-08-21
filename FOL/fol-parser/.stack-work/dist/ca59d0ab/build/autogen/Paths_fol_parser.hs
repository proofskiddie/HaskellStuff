{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fol_parser (
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

bindir     = "C:\\Users\\skidd9\\Desktop\\Languages\\Haskell\\FOL\\fol-parser\\.stack-work\\install\\7abedeba\\bin"
libdir     = "C:\\Users\\skidd9\\Desktop\\Languages\\Haskell\\FOL\\fol-parser\\.stack-work\\install\\7abedeba\\lib\\x86_64-windows-ghc-8.0.2\\fol-parser-0.1.0.0-AUzyGf7to42LhtBFMfu8mu"
dynlibdir  = "C:\\Users\\skidd9\\Desktop\\Languages\\Haskell\\FOL\\fol-parser\\.stack-work\\install\\7abedeba\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\skidd9\\Desktop\\Languages\\Haskell\\FOL\\fol-parser\\.stack-work\\install\\7abedeba\\share\\x86_64-windows-ghc-8.0.2\\fol-parser-0.1.0.0"
libexecdir = "C:\\Users\\skidd9\\Desktop\\Languages\\Haskell\\FOL\\fol-parser\\.stack-work\\install\\7abedeba\\libexec"
sysconfdir = "C:\\Users\\skidd9\\Desktop\\Languages\\Haskell\\FOL\\fol-parser\\.stack-work\\install\\7abedeba\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fol_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fol_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fol_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fol_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fol_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fol_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
