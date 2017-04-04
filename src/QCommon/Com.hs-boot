module QCommon.Com where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Types
import QCommon.XCommandT

initializeArgv :: [String] -> Quake ()

comError :: Int -> B.ByteString -> Quake ()

printf :: B.ByteString -> Quake ()

println :: B.ByteString -> Quake ()

dprintf :: B.ByteString -> Quake ()

argc :: Quake Int

argv :: Int -> Quake B.ByteString

clearArgv :: Int -> Quake ()

errorF :: XCommandT

parse :: B.ByteString -> Int -> Int -> Quake (Maybe B.ByteString, Int)

chktbl :: UV.Vector Word8

blockSequenceCRCByte :: B.ByteString -> Int -> Int -> Int -> Quake Word8

stripExtension :: B.ByteString -> B.ByteString

quit :: Quake ()

fatalError :: B.ByteString -> Quake ()