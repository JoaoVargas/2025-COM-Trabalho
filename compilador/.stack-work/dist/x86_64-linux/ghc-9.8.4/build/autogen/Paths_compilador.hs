{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_compilador (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/jotapc/Projects/Repo-Udesc-Geral/Disciplinas/2025.1/2025.1.COM/Trabalho/compilador/.stack-work/install/x86_64-linux/9d8a6ca8b08b3a32233572bc5a1aeea312ec53d74e5ec8eb18f6f7c3834f1478/9.8.4/bin"
libdir     = "/home/jotapc/Projects/Repo-Udesc-Geral/Disciplinas/2025.1/2025.1.COM/Trabalho/compilador/.stack-work/install/x86_64-linux/9d8a6ca8b08b3a32233572bc5a1aeea312ec53d74e5ec8eb18f6f7c3834f1478/9.8.4/lib/x86_64-linux-ghc-9.8.4/compilador-0.1.0.0-Dgd12S9zGj09k1U26D3Gne"
dynlibdir  = "/home/jotapc/Projects/Repo-Udesc-Geral/Disciplinas/2025.1/2025.1.COM/Trabalho/compilador/.stack-work/install/x86_64-linux/9d8a6ca8b08b3a32233572bc5a1aeea312ec53d74e5ec8eb18f6f7c3834f1478/9.8.4/lib/x86_64-linux-ghc-9.8.4"
datadir    = "/home/jotapc/Projects/Repo-Udesc-Geral/Disciplinas/2025.1/2025.1.COM/Trabalho/compilador/.stack-work/install/x86_64-linux/9d8a6ca8b08b3a32233572bc5a1aeea312ec53d74e5ec8eb18f6f7c3834f1478/9.8.4/share/x86_64-linux-ghc-9.8.4/compilador-0.1.0.0"
libexecdir = "/home/jotapc/Projects/Repo-Udesc-Geral/Disciplinas/2025.1/2025.1.COM/Trabalho/compilador/.stack-work/install/x86_64-linux/9d8a6ca8b08b3a32233572bc5a1aeea312ec53d74e5ec8eb18f6f7c3834f1478/9.8.4/libexec/x86_64-linux-ghc-9.8.4/compilador-0.1.0.0"
sysconfdir = "/home/jotapc/Projects/Repo-Udesc-Geral/Disciplinas/2025.1/2025.1.COM/Trabalho/compilador/.stack-work/install/x86_64-linux/9d8a6ca8b08b3a32233572bc5a1aeea312ec53d74e5ec8eb18f6f7c3834f1478/9.8.4/etc"

getBinDir     = catchIO (getEnv "compilador_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "compilador_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "compilador_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "compilador_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compilador_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "compilador_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
