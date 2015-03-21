{-# LANGUAGE Rank2Types #-}
module QCommon.MSG where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState

writeCharI :: QuakeLens SizeBufT -> Int -> Quake ()
writeCharI = undefined -- TODO

writeCharF :: QuakeLens SizeBufT -> Float -> Quake ()
writeCharF = undefined -- TODO

writeByteI :: QuakeLens SizeBufT -> Int -> Quake ()
writeByteI = undefined -- TODO

writeByteF :: QuakeLens SizeBufT -> Float -> Quake ()
writeByteF = undefined -- TODO

writeShort :: QuakeLens SizeBufT -> Int -> Quake ()
writeShort = undefined -- TODO

writeInt :: QuakeLens SizeBufT -> Int -> Quake ()
writeInt = undefined -- TODO

writeLong :: QuakeLens SizeBufT -> Int -> Quake ()
writeLong = undefined -- TODO

writeFloat :: QuakeLens SizeBufT -> Float -> Quake ()
writeFloat = undefined -- TODO

writeString :: QuakeLens SizeBufT -> B.ByteString -> Quake ()
writeString = undefined -- TODO

writeCoord :: QuakeLens SizeBufT -> Float -> Quake ()
writeCoord = undefined -- TODO

writePos :: QuakeLens SizeBufT -> V3 Float -> Quake ()
writePos = undefined -- TODO

writeAngle :: QuakeLens SizeBufT -> Float -> Quake ()
writeAngle = undefined -- TODO

writeAngle16 :: QuakeLens SizeBufT -> Float -> Quake ()
writeAngle16 = undefined -- TODO
