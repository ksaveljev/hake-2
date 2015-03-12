{-# LANGUAGE TemplateHaskell #-}
module Render.MTexInfoT where

import Linear.V4 (V4)
import Control.Lens (makeLenses)

import Render.ImageT

data MTexInfoT =
  MTexInfoT { _mtiVecs      :: (V4 Float, V4 Float)
            , _mtiFlags     :: Int
            , _mtiNumFrames :: Int
            , _mtiNext      :: MTexInfoT
            , _mtiImage     :: ImageT
            }

makeLenses ''MTexInfoT
