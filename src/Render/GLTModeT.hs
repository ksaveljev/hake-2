{-# LANGUAGE TemplateHaskell #-}
module Render.GLTModeT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Types

data GLTModeT =
  GLTModeT { _gltmName :: B.ByteString
           , _gltmMode :: Int
           }

makeLenses ''GLTModeT
