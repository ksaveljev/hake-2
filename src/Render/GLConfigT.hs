{-# LANGUAGE TemplateHaskell #-}
module Render.GLConfigT
  ( module Render.GLConfigT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''GLConfigT

newGLConfigT :: GLConfigT
newGLConfigT =
  GLConfigT { _glcRenderer         = 0
            , _glcRendererString   = ""
            , _glcVendorString     = ""
            , _glcVersionString    = ""
            , _glcExtensionsString = ""
            , _glcAllowCds         = False
            , _glcVersion          = 1.1
            }