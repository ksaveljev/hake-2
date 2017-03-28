{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.TgaT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Word (Word8, Word16)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

data TgaT =
  TgaT { _tgaIdLength       :: Word8
       , _tgaColorMapType   :: Word8
       , _tgaImageType      :: Word8
       , _tgaColorMapIndex  :: Word16
       , _tgaColorMapLength :: Word16
       , _tgaColorMapSize   :: Word8
       , _tgaXOrigin        :: Word16
       , _tgaYOrigin        :: Word16
       , _tgaWidth          :: Word16
       , _tgaHeight         :: Word16
       , _tgaPixelSize      :: Word8
       , _tgaAttributes     :: Word8
       , _tgaData           :: BL.ByteString -- (un)compressed data
       }

makeLenses ''TgaT

newTgaT :: BL.ByteString -> TgaT
newTgaT = runGet getTgaT
  where getTgaT :: Get TgaT
        getTgaT = TgaT <$> getWord8
                       <*> getWord8
                       <*> getWord8
                       <*> getWord16le
                       <*> getWord16le
                       <*> getWord8
                       <*> getWord16le
                       <*> getWord16le
                       <*> getWord16le
                       <*> getWord16le
                       <*> getWord8
                       <*> getWord8
                       <*> getRemainingLazyByteString
