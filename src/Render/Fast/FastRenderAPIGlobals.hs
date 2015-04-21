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
import qualified Render.OpenGL.QGLConstants as QGLConstants

makeLenses ''FastRenderAPIGlobals

initialFastRenderAPIGlobals :: FastRenderAPIGlobals
initialFastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin        = 0
                       , _frGLDepthMax        = 0
                       , _frGLConfig          = newGLConfigT
                       , _frGLState           = newGLStateT
                       , _frd8to24table       = UV.replicate 256 0
                       , _frVid               = newVidDefT
                       , _frColorTableEXT     = False
                       , _frActiveTextureARB  = False
                       , _frPointParameterEXT = False
                       , _frLockArraysEXT     = False
                       , _frSwapIntervalEXT   = False
                       , _frTexture0          = QGLConstants.glTexture0
                       , _frTexture1          = QGLConstants.glTexture1
                       , _frGLFilterMin       = QGLConstants.glLinearMipmapNearest
                       , _frGLFilterMax       = QGLConstants.glLinear
                       }
