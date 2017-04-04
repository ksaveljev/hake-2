{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DNodeT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

dNodeTSize :: Int
dNodeTSize = 4 + 8 + 6 + 6 + 2 + 2

makeLenses ''DNodeT

newDNodeT :: BL.ByteString -> DNodeT
newDNodeT = runGet getDNodeT

getDNodeT :: Get DNodeT
getDNodeT = DNodeT <$> getInt
                   <*> getInt2
                   <*> getV3Int16
                   <*> getV3Int16
                   <*> getWord16le
                   <*> getWord16le
