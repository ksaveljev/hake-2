module Game.TraceT where

import Linear.V3 (V3)

import Game.CPlaneT
import Game.CSurfaceT
import Game.EdictT

data TraceT =
  TraceT { tAllSolid   :: Bool
         , tStartSolid :: Bool
         , tFraction   :: Float
         , tEndPos     :: V3 Float
         , tPlane      :: CPlaneT
         , tSurface    :: CSurfaceT
         , tContents   :: Int
         , tEnt        :: EdictT
         }
