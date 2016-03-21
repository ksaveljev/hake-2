{-# LANGUAGE Rank2Types #-}
module QCommon.MSG
  ( beginReading
  , readShort
  , readLong
  , writeByteI
  , writeDeltaUserCmd
  , writeInt
  , writeLong
  , writeString
  ) where

import qualified QCommon.Com as Com
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           Types

import           Control.Lens (Traversal', Lens', use, (^.), (.=), (+=))
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import           Data.Int (Int16, Int32)
import           Data.Word (Word8, Word16, Word32)

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI sizeBufLens c = SZ.write sizeBufLens (B.pack [c']) 1
  where c' = fromIntegral (c .&. 0xFF) :: Word8

writeInt :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeInt sizeBufLens v = do -- IMPROVE?
    let v' = fromIntegral v :: Word32
        a = fromIntegral (v' .&. 0xFF) :: Word8
        b = fromIntegral ((v' `shiftR` 8) .&. 0xFF) :: Word8
        c = fromIntegral ((v' `shiftR` 16) .&. 0xFF) :: Word8
        d = fromIntegral ((v' `shiftR` 24) .&. 0xFF) :: Word8
    SZ.write sizeBufLens (B.pack [a, b, c, d]) 4

writeLong :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeLong sizeBufLens v = writeInt sizeBufLens v

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens str = do
  SZ.write sizeBufLens str (B.length str)
  writeByteI sizeBufLens 0

beginReading :: Lens' QuakeState SizeBufT -> Quake ()
beginReading sizeBufLens = sizeBufLens.sbReadCount .= 0

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
        b' = (fromIntegral b) `shiftL` 8 :: Word16
        result = fromIntegral ((fromIntegral a) .|. b') :: Int16

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

writeDeltaUserCmd :: Traversal' QuakeState SizeBufT -> UserCmdT -> UserCmdT -> Quake ()
writeDeltaUserCmd = error "MSG.writeDeltaUserCmd" -- TODO