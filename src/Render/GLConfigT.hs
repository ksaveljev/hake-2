{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.GLConfigT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data GLConfigT =
  GLConfigT { _glcRenderer         :: Int
            , _glcRendererString   :: B.ByteString
            , _glcVendorString     :: B.ByteString
            , _glcVersionString    :: B.ByteString
            , _glcExtensionsString :: B.ByteString
            , _glcAllowCds         :: Bool
            , _glcVersion          :: Float
            }

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
