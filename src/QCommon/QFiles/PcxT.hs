{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.PcxT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int8)
import Data.Word (Word16)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data PcxT =
  PcxT { _pcxManufacturer :: Int8
       , _pcxVersion      :: Int8
       , _pcxEncoding     :: Int8
       , _pcxBitsPerPixel :: Int8
       , _pcxXMin         :: Word16
       , _pcxYMin         :: Word16
       , _pcxXMax         :: Word16
       , _pcxYMax         :: Word16
       , _pcxHRes         :: Word16
       , _pcxVRes         :: Word16
       , _pcxPalette      :: B.ByteString -- size 48
       , _pcxReserved     :: Int8
       , _pcxColorPlanes  :: Int8
       , _pcxBytesPerLine :: Word16
       , _pcxPaletteType  :: Word16
       , _pcxFiller       :: B.ByteString -- size 58
       , _pcxData         :: BL.ByteString -- unbounded
       }

makeLenses ''PcxT

newPcxT :: BL.ByteString -> PcxT
newPcxT = runGet getPcx
  where getPcx :: Get PcxT
        getPcx = PcxT <$> getInt8
                      <*> getInt8
                      <*> getInt8
                      <*> getInt8
                      <*> getWord16
                      <*> getWord16
                      <*> getWord16
                      <*> getWord16
                      <*> getWord16
                      <*> getWord16
                      <*> getByteString 48
                      <*> getInt8
                      <*> getInt8
                      <*> getWord16
                      <*> getWord16
                      <*> getByteString 58
                      <*> getRemainingLazyByteString

        getInt8 :: Get Int8
        getInt8 = fromIntegral <$> getWord8

        getWord16 :: Get Word16
        getWord16 = getWord16le
