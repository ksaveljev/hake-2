{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DLeafT
  ( module QCommon.QFiles.BSP.DLeafT
  ) where

import Types
import Util.Binary (getInt, getInt16, getV3Int16)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get, getWord16le)

makeLenses ''DLeafT

dLeafTSize :: Int
dLeafTSize = 4 + 8 * 2 + 4 * 2

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
