{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DLeafT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

dLeafTSize :: Int
dLeafTSize = 4 + 8 * 2 + 4 * 2

makeLenses ''DLeafT

newDLeafT :: BL.ByteString -> DLeafT
newDLeafT = runGet getDLeafT

getDLeafT :: Get DLeafT
getDLeafT = DLeafT <$> getInt
                   <*> getInt16
                   <*> getInt16
                   <*> getV3Int16
                   <*> getV3Int16
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
