{-# LANGUAGE Rank2Types #-}
module QCommon.MSG
  ( writeByteI
  , writeString
  ) where

import qualified QCommon.SZ as SZ
import           Types

import           Control.Lens (Traversal')
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import           Data.Word (Word8)

writeByteI :: Traversal' QuakeState SizeBufT -> Int -> Quake ()
writeByteI sizeBufLens c = SZ.write sizeBufLens (B.pack [c']) 1
  where c' = fromIntegral (c .&. 0xFF) :: Word8

writeString :: Traversal' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString sizeBufLens str = do
  SZ.write sizeBufLens str (B.length str)
  writeByteI sizeBufLens 0
