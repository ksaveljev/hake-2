{-# LANGUAGE Rank2Types #-}
module QCommon.MSG
  ( beginReading
  , readAngle
  , readByte
  , readCoord
  , readLong
  , readPos
  , readShort
  , readString
  , readStringLine
  , writeByteI
  , writeCharI
  , writeDeltaUserCmd
  , writeInt
  , writeLong
  , writeShort
  , writeString
  ) where

import qualified QCommon.Com as Com
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           Types

import           Control.Lens (Traversal', Lens', use, (^.), (.=), (+=))
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int8, Int16, Int32)
import           Data.Word (Word8, Word16, Word32)
import           Linear (V3(..))

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI sizeBufLens c = SZ.write sizeBufLens (B.pack [c']) 1
  where c' = fromIntegral (c .&. 0xFF) :: Word8

writeCharI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeCharI = writeByteI

writeInt :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeInt sizeBufLens v = -- IMPROVE ?
  do let v' = fromIntegral v :: Word32
         a = fromIntegral (v' .&. 0xFF) :: Word8
         b = fromIntegral ((v' `shiftR` 8) .&. 0xFF) :: Word8
         c = fromIntegral ((v' `shiftR` 16) .&. 0xFF) :: Word8
         d = fromIntegral ((v' `shiftR` 24) .&. 0xFF) :: Word8
     SZ.write sizeBufLens (B.pack [a, b, c, d]) 4

writeShort :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeShort sizeBufLens c =
  do let c' = fromIntegral c :: Word32
         a = fromIntegral (c' .&. 0xFF) :: Word8
         b = fromIntegral ((c' `shiftR` 8) .&. 0xFF) :: Word8
     SZ.write sizeBufLens (B.pack [a, b]) 2

writeLong :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeLong = writeInt

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens str = do
  SZ.write sizeBufLens str (B.length str)
  writeByteI sizeBufLens 0

beginReading :: Lens' QuakeState SizeBufT -> Quake ()
beginReading sizeBufLens = sizeBufLens.sbReadCount .= 0

readByte :: Lens' QuakeState SizeBufT -> Quake Int
readByte sizeBufLens = doReadByte sizeBufLens =<< use sizeBufLens

doReadByte :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int
doReadByte sizeBufLens sizeBuf =
  do sizeBufLens.sbReadCount += 1
     return c
  where c | (sizeBuf^.sbReadCount) + 1 > (sizeBuf^.sbCurSize) = -1
          | otherwise = fromIntegral ((sizeBuf^.sbData) `B.index` (sizeBuf^.sbReadCount))

readShort :: Lens' QuakeState SizeBufT -> Quake Int
readShort sizeBufLens = doReadShort sizeBufLens =<< use sizeBufLens

doReadShort :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int
doReadShort sizeBufLens sizeBuf
  | (sizeBuf^.sbReadCount) + 2 > (sizeBuf^.sbCurSize) =
      do Com.printf "buffer underrun in ReadShort!"
         return (-1)
  | otherwise =
      do sizeBufLens.sbReadCount += 2
         return (fromIntegral result)
  where buf = sizeBuf^.sbData
        readCount = sizeBuf^.sbReadCount
        a = B.index buf readCount
        b = B.index buf (readCount + 1)
        b' = fromIntegral b `shiftL` 8 :: Word16
        result = fromIntegral (fromIntegral a .|. b') :: Int16

readLong :: Lens' QuakeState SizeBufT -> Quake Int
readLong sizeBufLens = doReadLong sizeBufLens =<< use sizeBufLens

doReadLong :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int
doReadLong sizeBufLens sizeBuf
  | (sizeBuf^.sbReadCount) + 4 > (sizeBuf^.sbCurSize) =
      do Com.printf "buffer underrun in ReadLong!"
         return (-1)
  | otherwise =
      do sizeBufLens.sbReadCount += 4
         return (fromIntegral result )
  where buf = sizeBuf^.sbData
        readCount = sizeBuf^.sbReadCount
        a = fromIntegral (B.index buf readCount) :: Word32
        b = fromIntegral (B.index buf (readCount + 1)) :: Word32
        c = fromIntegral (B.index buf (readCount + 2)) :: Word32
        d = fromIntegral (B.index buf (readCount + 3)) :: Word32
        result = fromIntegral (a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)) :: Int32

readStringLine :: Lens' QuakeState SizeBufT -> Quake B.ByteString
readStringLine sizeBufLens =
  do str <- fmap trim (readStr sizeBufLens 0 mempty)
     Com.dprintf (B.concat ["MSG.ReadStringLine:[", str, "]\n"])
     return str
  where trim = B.reverse . BC.dropWhile (<= ' ') . B.reverse -- IMPROVE ?

readStr :: Lens' QuakeState SizeBufT -> Int -> BB.Builder -> Quake B.ByteString
readStr sizeBufLens idx acc
  | idx >= 2047 = return (BL.toStrict (BB.toLazyByteString acc))
  | otherwise =
      do c <- readChar sizeBufLens
         processChar c
  where processChar c
          | c `elem` [-1, 0, 0x0A] =
              return (BL.toStrict (BB.toLazyByteString acc))
          | otherwise = readStr sizeBufLens (idx + 1) (acc `mappend` BB.int8 c)

readString :: Lens' QuakeState SizeBufT -> Quake B.ByteString
readString sizeBufLens = buildString 0 mempty
  where buildString len acc
          | len >= 2047 = return (BL.toStrict (BB.toLazyByteString acc))
          | otherwise = do
              c <- readByte sizeBufLens
              processChar len acc c
        processChar len acc c
          | c `elem` [-1, 0] =
              return (BL.toStrict (BB.toLazyByteString acc))
          | otherwise = buildString (len + 1) (acc `mappend` BB.word8 (fromIntegral c))

readChar :: Lens' QuakeState SizeBufT -> Quake Int8
readChar sizeBufLens = doReadChar sizeBufLens =<< use sizeBufLens

doReadChar :: Lens' QuakeState SizeBufT -> SizeBufT -> Quake Int8
doReadChar sizeBufLens msgRead =
  do sizeBufLens.sbReadCount += 1
     return c
  where c | (msgRead^.sbReadCount) + 1 > (msgRead^.sbCurSize) = -1
          | otherwise = fromIntegral (B.index (msgRead^.sbData) (msgRead^.sbReadCount))

readPos :: Lens' QuakeState SizeBufT -> Quake (V3 Float)
readPos sizeBufLens = V3 <$> readCoord sizeBufLens
                         <*> readCoord sizeBufLens
                         <*> readCoord sizeBufLens

readCoord :: Lens' QuakeState SizeBufT -> Quake Float
readCoord sizeBufLens = fmap ((* (1.0 / 8.0)) . fromIntegral) (readShort sizeBufLens)

readAngle :: Lens' QuakeState SizeBufT -> Quake Float
readAngle sizeBufLens = fmap ((* (360.0 / 256)) . fromIntegral) (readChar sizeBufLens)

writeDeltaUserCmd :: Traversal' QuakeState SizeBufT -> UserCmdT -> UserCmdT -> Quake ()
writeDeltaUserCmd = error "MSG.writeDeltaUserCmd" -- TODO
