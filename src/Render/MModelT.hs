{-# LANGUAGE TemplateHaskell #-}
module Render.MModelT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

data MModelT =
  MModelT { _mmMins      :: V3 Float
          , _mmMaxs      :: V3 Float
          , _mmOrigin    :: V3 Float
          , _mmRadius    :: Float
          , _mmHeadNode  :: Int
          , _mmVisLeafs  :: Int
          , _mmFirstFace :: Int
          , _mmNumFaces  :: Int
          }

makeLenses ''MModelT
