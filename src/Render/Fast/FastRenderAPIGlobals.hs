{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.FastRenderAPIGlobals ( module Render.Fast.FastRenderAPIGlobals
                                        , module Render.GLConfigT
                                        , module Render.GLStateT
                                        ) where

import Control.Lens (makeLenses)

import Internal
import Render.GLConfigT
import Render.GLStateT

makeLenses ''FastRenderAPIGlobals

initialFastRenderAPIGlobals :: FastRenderAPIGlobals
initialFastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin = 0
                       , _frGLDepthMax = 0
                       , _frGLConfig   = newGLConfigT
                       , _frGLState    = newGLStateT
                       }
