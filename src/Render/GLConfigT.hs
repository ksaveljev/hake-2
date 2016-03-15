{-# LANGUAGE TemplateHaskell #-}
module Render.GLConfigT
  ( module Render.GLConfigT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''GLConfigT

newGLConfigT :: GLConfigT
newGLConfigT =
  GLConfigT { _glcRenderer         = 0
            , _glcRendererString   = B.empty
            , _glcVendorString     = B.empty
            , _glcVersionString    = B.empty
            , _glcExtensionsString = B.empty
            , _glcAllowCds         = False
            , _glcVersion          = 1.1
            }