{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DPlaneT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Binary.IEEE754 (getFloat32le)
import Data.Functor ((<$>))
import Linear (V3(..))
import qualified Data.ByteString.Lazy as BL

data DPlaneT =
  DPlaneT { _dpNormal :: V3 Float
          , _dpDist   :: Float
          , _dpType   :: Int
          }

makeLenses ''DPlaneT

newDPLaneT :: BL.ByteString -> DPlaneT
newDPLaneT = runGet getDPlaneT
  where getDPlaneT :: Get DPlaneT
        getDPlaneT = DPlaneT <$> getV3Float
                             <*> getFloat32le
                             <*> getInt

        getV3Float :: Get (V3 Float)
        getV3Float = V3 <$> getFloat32le
                        <*> getFloat32le
                        <*> getFloat32le

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le
