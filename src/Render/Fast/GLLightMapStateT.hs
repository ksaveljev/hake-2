{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.GLLightMapStateT
  ( module Render.Fast.GLLightMapStateT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

makeLenses ''GLLightMapStateT

blockWidth :: Int
blockWidth = 128

newGLLightMapStateT :: GLLightMapStateT
newGLLightMapStateT =
  GLLightMapStateT { _lmsInternalFormat         = 0
                   , _lmsCurrentLightmapTexture = 0
                   , _lmsAllocated              = UV.replicate blockWidth 0
                   }