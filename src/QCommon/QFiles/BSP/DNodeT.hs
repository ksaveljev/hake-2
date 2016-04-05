{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DNodeT
  ( module QCommon.QFiles.BSP.DNodeT
  ) where

import Types
import Util.Binary (getInt, getInt2, getV3Int16)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get, getWord16le)

makeLenses ''DNodeT

dNodeTSize :: Int
dNodeTSize = 4 + 8 + 6 + 6 + 2 + 2

getDNodeT :: Get DNodeT
getDNodeT = DNodeT <$> getInt
                   <*> getInt2
                   <*> getV3Int16
                   <*> getV3Int16
                   <*> getWord16le
                   <*> getWord16le