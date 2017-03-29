{-# LANGUAGE TemplateHaskell #-}
module Render.GLConfigT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Types

makeLenses ''GLConfigT

newGLConfigT :: GLConfigT
newGLConfigT = GLConfigT
    { _glcRenderer         = 0
    , _glcRendererString   = ""
    , _glcVendorString     = ""
    , _glcVersionString    = ""
    , _glcExtensionsString = ""
    , _glcAllowCds         = False
    , _glcVersion          = 1.1
    }
