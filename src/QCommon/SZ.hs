{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.SZ where

import Data.Word (Word8)
import Control.Monad (when, unless)
import Control.Lens ((^.), (%=), use, (.=))
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

import Quake
import QuakeState
import qualified QCommon.Com as Com

init :: UV.Vector Word8 -> Int -> (SizeBufT -> Quake a) -> Quake a
init bufData len f =
    let buf = SizeBufT False False bufData len 0 0
    in f buf

clear :: SizeBufTLens -> Quake ()
clear bufLens = do
    bufLens.sbCurSize .= 0
    bufLens.sbOverflowed .= False

getSpace :: SizeBufTLens -> Int -> Quake Int
getSpace bufLens len = do
    buf <- use bufLens

    when (buf^.sbCurSize + len > buf^.sbMaxSize) $
      do
        unless (buf^.sbAllowOverflow) $ Com.comError undefined undefined -- TODO
        when (len > buf^.sbMaxSize) $ Com.comError undefined undefined -- TODO

        Com.printf "SZ.getSpace: overflow\n"
        clear bufLens
        bufLens.sbOverflowed .= True

    let oldsize = buf^.sbCurSize
    bufLens.sbCurSize .= oldsize + len

    return oldsize

write :: SizeBufTLens -> B.ByteString -> Int -> Quake ()
write bufLens bufData len = do
    idx <- getSpace bufLens len
    bufLens.sbData %= (UV.// ([idx..] `zip` B.unpack bufData))
