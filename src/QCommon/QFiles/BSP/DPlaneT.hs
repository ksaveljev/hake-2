{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DPlaneT
  ( module QCommon.QFiles.BSP.DPlaneT
  ) where

import Types
import Util.Binary (getV3Float, getInt)

import Control.Lens (makeLenses)
import Data.Binary.Get (Get)
import Data.Binary.IEEE754 (getFloat32le)

makeLenses ''DPlaneT

dPlaneTSize :: Int
dPlaneTSize = 3 * 4 + 4 + 4

getDPlaneT :: Get DPlaneT
getDPlaneT = DPlaneT <$> getV3Float <*> getFloat32le <*> getInt
