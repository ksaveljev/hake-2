{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DLeafT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

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
