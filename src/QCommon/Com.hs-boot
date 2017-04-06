module QCommon.Com
    ( argv
    , blockSequenceCRCByte
    , clearArgv
    , comError
    , dprintf
    , fatalError
    , initializeArgv
    , parse
    , printf
    , quit
    ) where

import qualified Data.ByteString as B
import           Data.Word       (Word8)

import           Types

argv :: Int -> Quake B.ByteString
blockSequenceCRCByte :: B.ByteString -> Int -> Int -> Int -> Quake Word8
clearArgv :: Int -> Quake ()
comError :: Int -> B.ByteString -> Quake ()
dprintf :: B.ByteString -> Quake ()
fatalError :: B.ByteString -> Quake ()
initializeArgv :: [String] -> Quake ()
parse :: B.ByteString -> Int -> Int -> Quake (Maybe B.ByteString, Int)
printf :: B.ByteString -> Quake ()
quit :: Quake ()
