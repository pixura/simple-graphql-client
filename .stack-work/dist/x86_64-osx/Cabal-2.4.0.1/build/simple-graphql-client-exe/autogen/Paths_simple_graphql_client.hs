{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_simple_graphql_client (
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

bindir     = "/Users/charlescrain/dev/simple-graphql-client/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/bin"
libdir     = "/Users/charlescrain/dev/simple-graphql-client/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/lib/x86_64-osx-ghc-8.6.5/simple-graphql-client-0.1.0.0-9CSKVjIXArN3qmy0fpDMY6-simple-graphql-client-exe"
dynlibdir  = "/Users/charlescrain/dev/simple-graphql-client/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/charlescrain/dev/simple-graphql-client/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/share/x86_64-osx-ghc-8.6.5/simple-graphql-client-0.1.0.0"
libexecdir = "/Users/charlescrain/dev/simple-graphql-client/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/libexec/x86_64-osx-ghc-8.6.5/simple-graphql-client-0.1.0.0"
sysconfdir = "/Users/charlescrain/dev/simple-graphql-client/.stack-work/install/x86_64-osx/lts-13.23/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "simple_graphql_client_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "simple_graphql_client_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "simple_graphql_client_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "simple_graphql_client_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_graphql_client_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "simple_graphql_client_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
