module Paths_con_lang_gen (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chaosrobie/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-6.14/7.10.3/bin"
libdir     = "/home/chaosrobie/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-6.14/7.10.3/lib/x86_64-linux-ghc-7.10.3/con-lang-gen-0.0.0.1-1eA9qWUPH9nBowcXqqaV8r"
datadir    = "/home/chaosrobie/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-6.14/7.10.3/share/x86_64-linux-ghc-7.10.3/con-lang-gen-0.0.0.1"
libexecdir = "/home/chaosrobie/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-6.14/7.10.3/libexec"
sysconfdir = "/home/chaosrobie/Documents/con-lang-gen/.stack-work/install/x86_64-linux/lts-6.14/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "con_lang_gen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "con_lang_gen_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "con_lang_gen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "con_lang_gen_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "con_lang_gen_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
