{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.GLLightMapStateT
    ( module Render.Fast.GLLightMapStateT
    ) where

import           Control.Lens                 (makeLenses)
import           Data.IORef                   (newIORef)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import           System.IO.Unsafe             (unsafePerformIO)

import qualified Constants
import           Render.MSurfaceT             (newMSurfaceT)
import           Types

makeLenses ''GLLightMapStateT

blockWidth :: Int
blockWidth = 128

blockHeight :: Int
blockHeight = 128

newGLLightMapStateT :: GLLightMapStateT
newGLLightMapStateT = GLLightMapStateT
    { _lmsInternalFormat         = 0
    , _lmsCurrentLightmapTexture = 0
    , _lmsAllocated              = UV.replicate blockWidth 0
    , _lmsLightmapSurfaces       = unsafePerformIO (V.replicateM Constants.maxLightMaps (newIORef newMSurfaceT))
    , _lmsLightmapBuffer         = unsafePerformIO (MSV.replicate (4 * blockWidth * blockHeight) 0)
    }