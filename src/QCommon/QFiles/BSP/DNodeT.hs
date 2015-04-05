{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DNodeT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

dNodeTSize :: Int
dNodeTSize = 4 + 8 + 6 + 6 + 2 + 2

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
