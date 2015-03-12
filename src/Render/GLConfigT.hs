{-# LANGUAGE TemplateHaskell #-}
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
            }

makeLenses ''GLConfigT
