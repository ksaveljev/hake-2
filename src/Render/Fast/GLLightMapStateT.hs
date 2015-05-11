{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.GLLightMapStateT where

import Control.Lens (makeLenses)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable.Mutable as MV

import Internal
import qualified Constants

dynamicLightWidth :: Int
dynamicLightWidth = 128

dynamicLightHeight :: Int
dynamicLightHeight = 128

lightmapBytes :: Int
lightmapBytes = 4

blockWidth :: Int
blockWidth = 128

blockHeight :: Int
blockHeight = 128

makeLenses ''GLLightMapStateT

newGLLightMapStateT :: GLLightMapStateT
newGLLightMapStateT =
  GLLightMapStateT { _lmsInternalFormat         = 0
                   , _lmsCurrentLightmapTexture = 0
                   , _lmsLightmapSurfaces       = V.replicate Constants.maxLightMaps (-1) -- TODO: reference ?
                   , _lmsAllocated              = UV.replicate blockWidth 0
                   , _lmsLightmapBuffer         = unsafePerformIO $ MV.new (4 * blockWidth * blockHeight)
                   }
