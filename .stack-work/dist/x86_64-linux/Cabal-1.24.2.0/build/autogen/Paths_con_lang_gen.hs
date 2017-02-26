{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_con_lang_gen (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/robert/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-8.2/8.0.2/bin"
libdir     = "/home/robert/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-8.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/con-lang-gen-0.0.0.1-1fM7O7MZmH036NvcEJweFm"
dynlibdir  = "/home/robert/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-8.2/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/robert/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-8.2/8.0.2/share/x86_64-linux-ghc-8.0.2/con-lang-gen-0.0.0.1"
libexecdir = "/home/robert/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-8.2/8.0.2/libexec"
sysconfdir = "/home/robert/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-8.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "con_lang_gen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "con_lang_gen_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "con_lang_gen_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "con_lang_gen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "con_lang_gen_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "con_lang_gen_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
