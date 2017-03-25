{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushSideT
    ( module QCommon.QFiles.BSP.DBrushSideT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get, getWord16le)

import           Types
import           Util.Binary     (getInt16)

makeLenses ''DBrushSideT

dBrushSideTSize :: Int
dBrushSideTSize = 4

getDBrushSideT :: Get DBrushSideT
getDBrushSideT = DBrushSideT <$> getWord16le <*> getInt16