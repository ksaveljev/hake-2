{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QCommon.MSG where

import Control.Lens (ASetter', Traversal', Lens', (.=), use, (^.), (+=))
import Data.Bits ((.&.), shiftR, shiftL, (.|.))
import Data.Int (Int16, Int32)
import Data.Word (Word8)
import Linear (V3, _x, _y, _z, dot)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.Com as Com
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

writeInt :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeInt sizeBufLens v = do
    let a :: Word8 = fromIntegral (v .&. 0xFF)
        b :: Word8 = fromIntegral ((v `shiftR` 8) .&. 0xFF)
        c :: Word8 = fromIntegral ((v `shiftR` 16) .&. 0xFF)
        d :: Word8 = fromIntegral ((v `shiftR` 24) .&. 0xFF)
    SZ.write sizeBufLens (B.pack [a, b, c, d]) 4

writeLong :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeLong _ _ = io (putStrLn "MSG.writeLong") >> undefined -- TODO

writeFloat :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeFloat _ _ = io (putStrLn "MSG.writeFloat") >> undefined -- TODO

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens s = do
    SZ.write sizeBufLens s (B.length s)

writeCoord :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeCoord _ _ = io (putStrLn "MSG.writeCoord") >> undefined -- TODO

writePos :: Traversal' QuakeState SizeBufT -> V3 Float -> Quake ()
writePos sizeBufLens pos = do
    writeShort sizeBufLens (truncate ((pos^._x) * 8))
    writeShort sizeBufLens (truncate ((pos^._y) * 8))
    writeShort sizeBufLens (truncate ((pos^._z) * 8))

writeDir :: Traversal' QuakeState SizeBufT -> V3 Float -> Quake ()
writeDir sizeBufLens dir = do
    -- do we need this?
    {-
        if (dir == null) {
            WriteByte(sb, 0);
            return;
        }
    -}
    let best = calcBest 0 0 0 Constants.numVertexNormals
    writeByteI sizeBufLens best

  where calcBest :: Float -> Int -> Int -> Int -> Int
        calcBest bestd best idx maxIdx
          | idx >= maxIdx = best
          | otherwise =
              let d = dot dir (Constants.byteDirs V.! idx)
              in if d > bestd
                   then calcBest d idx (idx + 1) maxIdx
                   else calcBest bestd best (idx + 1) maxIdx

writeAngle :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle _ _ = io (putStrLn "MSG.writeAngle") >> undefined -- TODO

writeAngle16 :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle16 _ _ = io (putStrLn "MSG.writeAngle16") >> undefined -- TODO

--
-- reading functions
--

beginReading :: Lens' QuakeState SizeBufT -> Quake ()
beginReading sizeBufLens =
    sizeBufLens.sbReadCount .= 0

-- IMPROVE: convert bytestring to int using binary package?
readLong :: Lens' QuakeState SizeBufT -> Quake Int
readLong sizeBufLens = do
    sizeBuf <- use $ sizeBufLens

    if (sizeBuf^.sbReadCount) + 4 > (sizeBuf^.sbCurSize)
      then do
        Com.printf "buffer underrun in ReadLong!"
        return (-1)
      else do
        let buf = sizeBuf^.sbData
            readCount = sizeBuf^.sbReadCount
            a :: Int32 = fromIntegral $ B.index buf readCount
            b :: Int32 = fromIntegral $ B.index buf (readCount + 1)
            c :: Int32 = fromIntegral $ B.index buf (readCount + 2)
            d :: Int32 = fromIntegral $ B.index buf (readCount + 3)
            result = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)
        sizeBufLens.sbReadCount += 4
        return $ fromIntegral result 

readShort :: Lens' QuakeState SizeBufT -> Quake Int16
readShort sizeBufLens = do
    sizeBuf <- use $ sizeBufLens

    if (sizeBuf^.sbReadCount) + 2 > (sizeBuf^.sbCurSize)
      then do
        Com.printf "buffer underrun in ReadLong!"
        return (-1)
      else do
        let buf = sizeBuf^.sbData
            readCount = sizeBuf^.sbReadCount
            a :: Int16 = fromIntegral $ B.index buf readCount
            b :: Int16 = fromIntegral $ B.index buf (readCount + 1)
            result = a .|. (b `shiftL` 8)
        sizeBufLens.sbReadCount += 2
        return $ fromIntegral result 
