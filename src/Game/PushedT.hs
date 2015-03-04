{-# LANGUAGE TemplateHaskell #-}
module Game.PushedT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.EdictT

data PushedT =
  PushedT { _pEnt      :: EdictT
          , _pOrigin   :: V3 Float
          , _pAngles   :: V3 Float
          , _pDeltaYaw :: Float
          }

makeLenses ''PushedT
