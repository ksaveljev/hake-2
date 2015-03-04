module Game.Pushed where

import Linear.V3 (V3)

import Game.Edict

data Pushed =
  Pushed { pushedEnt      :: Edict
         , pushedOrigin   :: V3 Float
         , pushedAngles   :: V3 Float
         , pushedDeltaYaw :: Float
         }
