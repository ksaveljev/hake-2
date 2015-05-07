{-# LANGUAGE TemplateHaskell #-}
module Render.Basic.BasicRenderAPIGlobals ( module Render.Basic.BasicRenderAPIGlobals
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
                        , _brMTexCoord2fSGIS      = False
                        , _brSelectTextureSGIS    = False
                        }
