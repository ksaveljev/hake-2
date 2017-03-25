{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DNodeT
    ( module QCommon.QFiles.BSP.DNodeT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get, getWord16le)

import           Types
import           Util.Binary     (getInt, getInt2, getV3Int16)

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