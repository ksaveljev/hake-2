{-# LANGUAGE TemplateHaskell #-}
module Render.GLStateT
    ( module Render.GLStateT
    ) where

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import           Types

makeLenses ''GLStateT

newGLStateT :: GLStateT
newGLStateT = GLStateT
    { _glsInverseIntensity        = 0
    , _glsFullScreen              = False
    , _glsPrevMode                = 0
    , _glsD16To8Table             = Nothing
    , _glsLightmapTextures        = 0
    , _glsCurrentTextures         = (0, 0)
    , _glsCurrentTmu              = 0
    , _glsCameraSeparation        = 0
    , _glsStereoEnabled           = False
    , _glsOriginalRedGammaTable   = UV.replicate 256 0
    , _glsOriginalGreenGammaTable = UV.replicate 256 0
    , _glsOriginalBlueGammaTable  = UV.replicate 256 0
    }