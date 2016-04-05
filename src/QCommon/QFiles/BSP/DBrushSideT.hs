{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushSideT
  ( module QCommon.QFiles.BSP.DBrushSideT
  ) where

import Types
import Util.Binary (getInt16)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get, getWord16le)

makeLenses ''DBrushSideT

dBrushSideTSize :: Int
dBrushSideTSize = 4

getDBrushSideT :: Get DBrushSideT
getDBrushSideT = DBrushSideT <$> getWord16le <*> getInt16