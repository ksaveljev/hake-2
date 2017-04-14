{-# LANGUAGE ScopedTypeVariables #-}
module Util.Lib
    ( atof
    , atoi
    , atov
    , crandom
    , fOpen
    , fOpenBinary
    , fClose
    , rand
    , randomF
    , tokenise
    , vtos
    ) where

import           Control.Exception     (handle, IOException)
import           Control.Lens          (use, (.=))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Int              (Int16)
import           Data.Maybe            (fromMaybe)
import           Linear                (V3(..))
import           System.IO             (Handle, IOMode)
import           System.IO             (openFile, hClose, openBinaryFile)
import           System.Random         (Random, random)
import           Text.Read             (readMaybe)

import           QuakeState
import           Types
import           Util.Binary           (encode)

atof :: B.ByteString -> Float
atof str
    | B.null str = 0.0
    | otherwise = fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE?

atoi :: B.ByteString -> Int
atoi str
    | B.null str = 0
    | otherwise = fromMaybe 0 (readMaybe (BC.unpack str)) -- IMPROVE?

-- IMPROVE
atov :: B.ByteString -> V3 Float
atov str =
    let strres = BC.split ' ' str
        len = length strres
        a = if len > 0 then atof (head strres) else 0
        b = if len > 1 then atof (head (tail strres)) else 0
        c = if len > 2 then atof (head (tail (tail strres))) else 0
    in V3 a b c

fOpenCommon :: (FilePath -> IOMode -> IO Handle) -> B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpenCommon f name mode = io (tryToOpenFile f name mode)

tryToOpenFile :: (FilePath -> IOMode -> IO Handle) -> B.ByteString -> IOMode -> IO (Maybe Handle)
tryToOpenFile f fileName openMode =
    handle (\(_ :: IOException) -> return Nothing) $ do
        h <- f (BC.unpack fileName) openMode
        return (Just h)

fOpen :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpen = fOpenCommon openFile

fOpenBinary :: B.ByteString -> IOMode -> Quake (Maybe Handle)
fOpenBinary = fOpenCommon openBinaryFile

fClose :: Handle -> Quake ()
fClose h = io (handle (\(_ :: IOException) -> return ()) (hClose h))

rand :: Quake Int16
rand = fmap abs rnd

randomF :: Quake Float
randomF = rnd

rnd :: (Random a, Num a) => Quake a
rnd = do
    g <- use (globals.gRnd)
    let (result, newG) = random g
    globals.gRnd .= newG
    return result

-- Like in libc
crandom :: Quake Float
crandom = do
    f <- randomF
    return ((f - 0.5) * 2)

tokenise :: B.ByteString -> B.ByteString -> [B.ByteString]
tokenise x y = let (h, t) = B.breakSubstring x y
               in h : if B.null t then [] else tokenise x (B.drop (B.length x) t)

vtos :: V3 Float -> B.ByteString
vtos (V3 a b c) = B.concat [encode a', " ", encode b', " ", encode c']
  where
    a' = truncate a :: Int
    b' = truncate b :: Int
    c' = truncate c :: Int
