{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.FastRenderAPIGlobals ( module Render.Fast.FastRenderAPIGlobals
                                        , module Client.VidDefT
                                        , module Render.GLConfigT
                                        , module Render.GLStateT
                                        , module Render.ImageT
                                        , module Render.ModelT
                                        ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Client.VidDefT
import Render.GLConfigT
import Render.GLStateT
import Render.ImageT
import Render.ModelT
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Render.OpenGL.QGLConstants as QGLConstants

makeLenses ''FastRenderAPIGlobals

initialFastRenderAPIGlobals :: FastRenderAPIGlobals
initialFastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin           = 0
                       , _frGLDepthMax           = 0
                       , _frGLConfig             = newGLConfigT
                       , _frGLState              = newGLStateT
                       , _frd8to24table          = UV.replicate 256 0
                       , _frVid                  = newVidDefT
                       , _frColorTableEXT        = False
                       , _frActiveTextureARB     = False
                       , _frPointParameterEXT    = False
                       , _frLockArraysEXT        = False
                       , _frSwapIntervalEXT      = False
                       , _frTexture0             = QGLConstants.glTexture0
                       , _frTexture1             = QGLConstants.glTexture1
                       , _frGLSolidFormat        = 3
                       , _frGLAlphaFormat        = 4
                       , _frGLTexSolidFormat     = 3
                       , _frGLTexAlphaFormat     = 4
                       , _frGLFilterMin          = QGLConstants.glLinearMipmapNearest
                       , _frGLFilterMax          = QGLConstants.glLinear
                       , _frNumGLTextures        = 0
                       , _frGLTextures           = V.generate RenderAPIConstants.maxGLTextures newImageT
                       , _frLastModes            = (-1, -1)
                       , _frRegistrationSequence = 0
                       , _frGammaTable           = B.replicate 256 0
                       , _frIntensityTable       = B.replicate 256 0
                       , _frModKnown             = V.empty
                       , _frModNoVis             = ""
                       , _frNoTexture            = ImageReference (-1)
                       , _frParticleTexture      = ImageReference (-1)
                       , _frUploadWidth          = 0
                       , _frUploadHeight         = 0
                       , _frUploadedPaletted     = False
                       }
