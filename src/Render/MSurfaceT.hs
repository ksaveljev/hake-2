{-# LANGUAGE TemplateHaskell #-}
module Render.MSurfaceT
  ( module Render.MSurfaceT
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

makeLenses ''MSurfaceT

newMSurfaceT :: MSurfaceT
newMSurfaceT =
  MSurfaceT { _msVisFrame           = 0
            , _msPlane              = Nothing
            , _msFlags              = 0
            , _msFirstEdge          = 0
            , _msNumEdges           = 0
            , _msTextureMins        = (-1, -1)
            , _msExtents            = (0, 0)
            , _msLightS             = 0
            , _msLightT             = 0
            , _msDLightS            = 0
            , _msDLightT            = 0
            , _msPolys              = Nothing
            , _msTextureChain       = Nothing
            , _msLightmapChain      = Nothing
            , _msTexInfo            = Ref (-1)
            , _msDLightFrame        = 0
            , _msDLightBits         = 0
            , _msLightmapTextureNum = 0
            , _msStyles             = B.replicate Constants.maxLightMaps 0
            , _msCachedLight        = UV.replicate Constants.maxLightMaps 0
            , _msSamples            = Nothing
            }