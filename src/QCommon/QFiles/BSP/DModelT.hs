{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DModelT
    ( module QCommon.QFiles.BSP.DModelT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get)

import           Types
import           Util.Binary     (getV3Float, getInt)

makeLenses ''DModelT

dModelTSize :: Int
dModelTSize = 3 * 4 + 3 * 4 + 3 * 4 + 4 + 4 + 4

getDModelT :: Get DModelT
getDModelT = DModelT <$> getV3Float
                     <*> getV3Float
                     <*> getV3Float
                     <*> getInt
                     <*> getInt
                     <*> getInt