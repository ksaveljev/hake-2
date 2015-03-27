{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DLeafT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import Linear (V3(..))
import qualified Data.ByteString.Lazy as BL

dLeafTSize :: Int
dLeafTSize = 4 + 8 * 2 + 4 * 2

data DLeafT =
  DLeafT { _dlContents       :: Int
         , _dlCluster        :: Int16
         , _dlArea           :: Int16
         , _dlMins           :: V3 Int16
         , _dlMaxs           :: V3 Int16
         , _dlFirstLeafFace  :: Word16
         , _dlNumLeafFaces   :: Word16
         , _dlFirstLeafBrush :: Word16
         , _dlNumLeafBrushes :: Word16
         }

makeLenses ''DLeafT

newDLeafT :: BL.ByteString -> DLeafT
newDLeafT = runGet getDLeafT
  where getDLeafT :: Get DLeafT
        getDLeafT = DLeafT <$> getInt
                           <*> getInt16
                           <*> getInt16
                           <*> getV3Int16
                           <*> getV3Int16
                           <*> getWord16le
                           <*> getWord16le
                           <*> getWord16le
                           <*> getWord16le

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

        getInt16 :: Get Int16
        getInt16 = fromIntegral <$> getWord16le

        getV3Int16 :: Get (V3 Int16)
        getV3Int16 = V3 <$> getInt16
                        <*> getInt16
                        <*> getInt16