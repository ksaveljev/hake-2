module QCommon.SZ where

import Data.Word (Word8)
import qualified Data.Vector.Unboxed as UV

import Quake
import QCommon.SizeBufT

init :: UV.Vector Word8 -> Int -> (SizeBufT -> Quake a) -> Quake a
init bufData len f =
    let buf = SizeBufT False False bufData len 0 0
    in f buf

clear :: SizeBufT -> (SizeBufT -> Quake a) -> Quake a
clear buf f =
    let clearedBuf = buf { _sbCurSize = 0, _sbOverflowed = False }
    in f clearedBuf
