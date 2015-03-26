{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DNodeT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import Linear (V3(..))
import qualified Data.ByteString.Lazy as BL

data DNodeT =
  DNodeT { _dnPlaneNum  :: Int
         , _dnChildren  :: (Int, Int)
         , _dnMins      :: V3 Int16
         , _dnMaxs      :: V3 Int16
         , _dnFirstFace :: Word16
         , _dnNumFaces  :: Word16
         }

makeLenses ''DNodeT

newDNodeT :: BL.ByteString -> DNodeT
newDNodeT = runGet getDNodeT
  where getDNodeT :: Get DNodeT
        getDNodeT = DNodeT <$> getInt
                           <*> getInt2
                           <*> getV3Int16
                           <*> getV3Int16
                           <*> getWord16le
                           <*> getWord16le

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

        getInt2 :: Get (Int, Int)
        getInt2 = (,) <$> getInt <*> getInt

        getV3Int16 :: Get (V3 Int16)
        getV3Int16 = V3 <$> getInt16
                        <*> getInt16
                        <*> getInt16

        getInt16 :: Get Int16
        getInt16 = fromIntegral <$> getWord16le
