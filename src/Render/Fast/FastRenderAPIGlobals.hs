{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.FastRenderAPIGlobals ( module Render.Fast.FastRenderAPIGlobals
                                        , module Client.VidDefT
                                        , module Render.GLConfigT
                                        , module Render.GLStateT
                                        ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Internal
import Client.VidDefT
import Render.GLConfigT
import Render.GLStateT

makeLenses ''FastRenderAPIGlobals

initialFastRenderAPIGlobals :: FastRenderAPIGlobals
initialFastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin  = 0
                       , _frGLDepthMax  = 0
                       , _frGLConfig    = newGLConfigT
                       , _frGLState     = newGLStateT
                       , _frd8to24table = UV.replicate 256 0
                       , _frVid         = newVidDefT
                       }
