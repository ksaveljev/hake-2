module Game.PushedT where

import Linear.V3 (V3)

import Game.EdictT

data PushedT =
  PushedT { pEnt      :: EdictT
          , pOrigin   :: V3 Float
          , pAngles   :: V3 Float
          , pDeltaYaw :: Float
          }
