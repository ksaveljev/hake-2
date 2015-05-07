{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.Basic.BasicRenderAPIGlobals ( module Render.Basic.BasicRenderAPIGlobals
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

makeLenses ''BasicRenderAPIGlobals

initialBasicRenderAPIGlobals :: BasicRenderAPIGlobals
initialBasicRenderAPIGlobals =
  BasicRenderAPIGlobals { _brGLConfig             = newGLConfigT
                        , _brGLState              = newGLStateT
                        , _brd8to24table          = UV.replicate 256 0
                        , _brVid                  = newVidDefT
                        , _brColorTableEXT        = False
                        , _brActiveTextureARB     = False
                        , _brPointParameterEXT    = False
                        , _brLockArraysEXT        = False
                        , _brSwapIntervalEXT      = False
                        , _brGLTexSolidFormat     = 0
                        , _brGLTexAlphaFormat     = 0
                        , _brMTexCoord2fSGIS      = False
                        , _brSelectTextureSGIS    = False
                        , _brGLFilterMin          = 0
                        , _brGLFilterMax          = 0
                        , _brNumGLTextures        = 0
                        , _brGLTextures           = V.generate RenderAPIConstants.maxGLTextures newImageT
                        , _brLastModes            = (0, 0)
                        , _brRegistrationSequence = 0
                        , _brGammaTable           = B.replicate 256 0
                        , _brIntensityTable       = B.replicate 256 0
                        , _brModKnown             = V.empty
                        , _brModNoVis             = ""
                        , _brDrawChars            = Nothing
                        }
