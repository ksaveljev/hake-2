{-# LANGUAGE ScopedTypeVariables #-}
module Util.Lib
  ( atof
  , fOpen
  , fOpenBinary
  , fClose
  ) where

import           Types

import           Control.Exception (handle, IOException)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (fromMaybe)
import           System.IO (Handle, IOMode, openFile, hClose, openBinaryFile)
import           Text.Read (readMaybe)

atof :: B.ByteString -> Float
atof str
  | B.null str = 0.0
  | otherwise = fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE?

fOpenCommon :: (FilePath -> IOMode -> IO Handle) -> B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpenCommon f name mode = request (io (tryToOpenFile f name mode))

tryToOpenFile :: (FilePath -> IOMode -> IO Handle) -> B.ByteString -> IOMode -> IO (Maybe Handle)
tryToOpenFile f fileName openMode =
  handle (\(_ :: IOException) -> return Nothing) $
    do h <- f (BC.unpack fileName) openMode
       return (Just h)

fOpen :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpen = fOpenCommon openFile

fOpenBinary :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpenBinary = fOpenCommon openBinaryFile

fClose :: Handle -> Quake ()
fClose h = request (io (handle (\(_ :: IOException) -> return ()) (hClose h)))