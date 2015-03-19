{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Lib where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Exception (handle, IOException)
import System.IO (Handle, IOMode, openFile, hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import qualified QCommon.Com as Com

atof :: B.ByteString -> Float
atof str = if B.length str == 0
             then 0.0
             else fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE: use binary package for conversion?

atoi :: B.ByteString -> Int
atoi str = if B.length str == 0
             then 0
             else fromMaybe 0 (readMaybe (BC.unpack str)) -- IMPROVE: use binary package for conversion?

fOpen :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpen name mode = do
    result <- io $ tryToOpenFile name mode
    case result of
      Nothing -> do
        Com.dprintf $ "Could not open file:" `B.append` name
        return Nothing
      h -> return h

  where tryToOpenFile :: B.ByteString -> IOMode -> IO (Maybe Handle)
        tryToOpenFile fileName openMode = handle (\(_ :: IOException) -> return Nothing) $ do
          h <- openFile (BC.unpack fileName) openMode
          return $ Just h

fClose :: Handle -> Quake ()
fClose = io . hClose
