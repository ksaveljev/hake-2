{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Lib where

import Control.Exception (handle, IOException)
import Control.Lens (use, (.=))
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import Linear (V3(..))
import System.IO (Handle, IOMode, openFile, hClose, openBinaryFile)
import System.Random (random)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified QCommon.Com as Com

atof :: B.ByteString -> Float
atof str = if B.length str == 0
             then 0.0
             else fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE: use binary package for conversion?

atoi :: B.ByteString -> Int
atoi str = if B.length str == 0
             then 0
             else fromMaybe 0 (readMaybe (BC.unpack str)) -- IMPROVE: use binary package for conversion?

-- Converts a string to a vector. Needs improvement
atov :: B.ByteString -> V3 Float
atov str = let strres = BC.split ' ' str
               len = length strres
               a = if len > 0 then atof (head strres) else 0
               b = if len > 1 then atof (head (tail strres)) else 0
               c = if len > 2 then atof (head (tail (tail strres))) else 0
           in V3 a b c

vtos :: V3 Float -> B.ByteString
vtos (V3 a b c) = BC.pack $ show (truncate a :: Int) ++ " " ++ show (truncate b :: Int) ++ " " ++ show (truncate c :: Int) -- IMPROVE ?

fOpenCommon :: (FilePath -> IOMode -> IO Handle) -> B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpenCommon f name mode = do
    result <- io $ tryToOpenFile name mode
    case result of
      Nothing -> do
        Com.dprintf $ "Could not open file:" `B.append` name
        return Nothing
      h -> return h

  where tryToOpenFile :: B.ByteString -> IOMode -> IO (Maybe Handle)
        tryToOpenFile fileName openMode = handle (\(_ :: IOException) -> return Nothing) $ do
          h <- f (BC.unpack fileName) openMode
          return $ Just h

fOpen :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpen = fOpenCommon openFile

fOpenBinary :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpenBinary = fOpenCommon openBinaryFile

fClose :: Handle -> Quake ()
fClose h = io $ handle (\(_ :: IOException) -> return ()) (hClose h)

rand :: Quake Int16
rand = do
    g <- use $ globals.rnd
    let (result, newG) = random g
    globals.rnd .= newG
    return $ if result < 0 then -result else result -- TODO: ugly hack, find different approach?

randomF :: Quake Float
randomF = do
    g <- use $ globals.rnd
    let (result, newG) = random g
    globals.rnd .= newG
    return result

-- Like in libc
crandom :: Quake Float
crandom = do
    f <- randomF
    return $ (f - 0.5) * 2
