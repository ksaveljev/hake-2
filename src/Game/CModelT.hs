module Game.CModelT where

import Linear.V3 (V3)

data CModelT =
  CModelT { cmMins     :: V3 Float
          , cmMaxs     :: V3 Float
          , cmOrigin   :: V3 Float
          , cmHeadNode :: Int
          }
