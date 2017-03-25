{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushT
    ( module QCommon.QFiles.BSP.DBrushT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get)

import           Types
import           Util.Binary     (getInt)

makeLenses ''DBrushT

dBrushTSize :: Int
dBrushTSize = 4 + 4 + 4

getDBrushT :: Get DBrushT
getDBrushT = DBrushT <$> getInt <*> getInt <*> getInt