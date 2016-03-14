{-# LANGUAGE ScopedTypeVariables #-}
module Util.Lib
  ( atof
  , atoi
  , fOpen
  , fOpenBinary
  , fClose
  , rand
  , vtos
  ) where

import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Exception (handle, IOException)
import           Control.Lens (use, (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int16)
import           Data.Maybe (fromMaybe)
import           Linear (V3(..))
import           System.IO (Handle, IOMode, openFile, hClose, openBinaryFile)
import           System.Random (random)
import           Text.Read (readMaybe)

atof :: B.ByteString -> Float
atof str
  | B.null str = 0.0
  | otherwise = fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE?

atoi :: B.ByteString -> Int
atoi str
  | B.null str = 0
  | otherwise = fromMaybe 0 (readMaybe (BC.unpack str)) -- IMPROVE?

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

rand :: Quake Int16
rand =
  do g <- use (globals.gRnd)
     let (result, newG) = random g
     globals.gRnd .= newG
     return (abs result)

vtos :: V3 Float -> B.ByteString
vtos (V3 a b c) = B.concat [encode a', " ", encode b', " ", encode c']
  where a' = truncate a :: Int
        b' = truncate b :: Int
        c' = truncate c :: Int