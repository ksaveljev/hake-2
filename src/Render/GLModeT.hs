{-# LANGUAGE TemplateHaskell #-}
module Render.GLModeT where

import Control.Lens (makeLenses)

import qualified Data.ByteString as B

data GLModeT =
  GLModeT { _glmName     :: B.ByteString
          , _glmMinimize :: Int
          , _glmMaximize :: Int
          }

makeLenses ''GLModeT
