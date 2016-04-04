{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushT
  ( module QCommon.QFiles.BSP.DBrushT
  ) where

import Types
import Util.Binary (getInt)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get)

makeLenses ''DBrushT

dBrushTSize :: Int
dBrushTSize = 4 + 4 + 4

getDBrushT :: Get DBrushT
getDBrushT = DBrushT <$> getInt <*> getInt <*> getInt
