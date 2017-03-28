{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DPlaneT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Linear (V3)
import qualified Data.ByteString.Lazy as BL

import Util.Binary

dPlaneTSize :: Int
dPlaneTSize = 3 * 4 + 4 + 4

data DPlaneT =
  DPlaneT { _dpNormal :: V3 Float
          , _dpDist   :: Float
          , _dpType   :: Int
          }

makeLenses ''DPlaneT

newDPlaneT :: BL.ByteString -> DPlaneT
newDPlaneT = runGet getDPlaneT

getDPlaneT :: Get DPlaneT
getDPlaneT = DPlaneT <$> getV3Float
                     <*> getFloat32le
                     <*> getInt
