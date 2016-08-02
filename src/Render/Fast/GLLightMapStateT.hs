{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.GLLightMapStateT
  ( module Render.Fast.GLLightMapStateT
  ) where

import qualified Constants
import           Render.MSurfaceT (newMSurfaceT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

makeLenses ''GLLightMapStateT

blockWidth :: Int
blockWidth = 128

blockHeight :: Int
blockHeight = 128

newGLLightMapStateT :: GLLightMapStateT
newGLLightMapStateT =
  GLLightMapStateT { _lmsInternalFormat         = 0
                   , _lmsCurrentLightmapTexture = 0
                   , _lmsAllocated              = UV.replicate blockWidth 0
                   , _lmsLightmapSurfaces       = V.replicate Constants.maxLightMaps newMSurfaceT
                   , _lmsLightmapBuffer         = SV.replicate (4 * blockWidth * blockHeight) 0
                   }
