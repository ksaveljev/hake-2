module Game.CPlaneT where

import Data.Int (Int8)
import Linear.V3 (V3)

data CPlaneT =
  CPlaneT { cpNormal   :: V3 Float
          , cpDist     :: Float
          , cpType     :: Int8
          , cpSignBits :: Int8
          , cpPad      :: (Int8, Int8)
          }
