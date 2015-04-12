{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QCommon.MSG where

import Control.Lens (ASetter', Traversal')
import Data.Bits ((.&.), shiftR)
import Data.Word (Word8)
import Linear.V3 (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState
import qualified QCommon.SZ as SZ

-- IMPROVE: use binary package for conversion to ByteString?

writeCharI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeCharI = writeByteI

writeCharF :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeCharF = writeByteF

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI sizeBufLens c = do
    let cw8 :: Word8 = fromIntegral (c .&. 0xFF)
    SZ.write sizeBufLens (B.pack [cw8]) 1

writeByteF :: Traversal' QuakeState SizeBufT -> Float -> Quake ()
writeByteF sizeBufLens c = do
    writeByteI sizeBufLens (truncate c)

writeShort :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeShort sizeBufLens c = do
    let a :: Word8 = fromIntegral (c .&. 0xFF)
        b :: Word8 = fromIntegral ((c `shiftR` 8) .&. 0xFF)
    SZ.write sizeBufLens (B.pack [a, b]) 2

writeInt :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeInt _ _ = io (putStrLn "MSG.writeInt") >> undefined -- TODO

writeLong :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeLong _ _ = io (putStrLn "MSG.writeLong") >> undefined -- TODO

writeFloat :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeFloat _ _ = io (putStrLn "MSG.writeFloat") >> undefined -- TODO

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens s = do
    SZ.write sizeBufLens s (B.length s)

writeCoord :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeCoord _ _ = io (putStrLn "MSG.writeCoord") >> undefined -- TODO

writePos :: ASetter' QuakeState SizeBufT -> V3 Float -> Quake ()
writePos _ _ = io (putStrLn "MSG.writePos") >> undefined -- TODO

writeDir :: ASetter' QuakeState SizeBufT -> V3 Float -> Quake ()
writeDir _ _ = io (putStrLn "MSG.writeDir") >> undefined -- TODO

writeAngle :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle _ _ = io (putStrLn "MSG.writeAngle") >> undefined -- TODO

writeAngle16 :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle16 _ _ = io (putStrLn "MSG.writeAngle16") >> undefined -- TODO
