{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Debug.Trace

import Control.Monad
import Control.Exception

import System.OsPath
import System.OsString
import System.Directory.OsPath

import System.OsString.Internal.Types
import System.Directory.Internal
import System.Posix.Internals hiding (withFilePath, peekFilePathLen)
-- import qualified System.Posix.Files.Common as Common
import Foreign

import System.Posix.PosixPath.FilePath
import System.Posix.Files hiding (getFileStatus, getSymbolicLinkStatus, createNamedPipe, createDevice, createLink, removeLink, createSymbolicLink, readSymbolicLink, rename, setOwnerAndGroup, setSymbolicLinkOwnerAndGroup, setFileTimes, setSymbolicLinkTimesHiRes, touchFile, touchSymbolicLink, setFileSize, getPathVar, setFileMode, fileAccess, fileExist, setFdTimesHiRes, setFileTimesHiRes)
import Data.ByteString.Short.Internal as BS

import GHC.Exts



import Foreign
import Foreign.C

main :: IO ()
main = do
  h <- getHomeDirectory
  found <- findFilesRecursivelyUpToFound (h) [osstr|node_modules|]
  print found
  print =<< mapM getSize found

-- Root, Needle(s), Result buffer, returns length of ret buffer
-- foreign import ccall "findFilesRecursivelyUpToFind" c_findFilesRecursivelyUpToFind :: CString -> CString -> Ptr CString -> IO CInt



-- findFilesRecursivelyUpToFound :: _ -> IO _



findFilesRecursivelyUpToFound :: OsPath -> OsString -> IO [OsPath]
findFilesRecursivelyUpToFound topdir needle = (do
  ls <- listDirectory topdir
  join <$>
    traverse (\f -> do
      let absolute_f = topdir </> f
      if f == needle
         then pure [absolute_f] -- No point in recursing down node_modules
         else do
            -- isDir <- fastIsDir absolute_f
            isDir <- fastIsDir absolute_f
            if isDir
               then findFilesRecursivelyUpToFound absolute_f needle -- `catch` (\e -> traceShow (e :: SomeException) (pure []))
               else pure []) ls)

-- FastIsDir is doomed because ShortByteString is not a null-terminated string, so the only way to call c_stat directly is by copying the contents to an extra-buffer.
-- So I'll be creating a C version of everything, since the solution is to have all filepaths be null-terminated strings
fastIsDir :: OsPath -> IO Bool
fastIsDir (getPosixString . getOsString -> SBS path) = do
  fp <- mallocForeignPtrBytes (144)
  withForeignPtr fp $ \p -> useAsCString (SBS path) $ \s -> c_stat s p
  -- let ptr = Ptr (byteArrayContents# path)
  -- withForeignPtr fp $ \p -> c_stat ptr p
  return (fileTypeIsDirectory $ fileTypeFromMetadata $ FileStatus fp)


getSize :: OsPath -> IO Integer
getSize p = do
  s     <- getFileSize p
  isDir <- doesDirectoryExist p
  if isDir
     then do
       ls <- listDirectory p
       (s +) . sum <$> traverse (getSize . (p </>)) ls
     else pure s
