module Game.Trace where

import Linear.V3 (V3)

import Game.CPlane
import Game.CSurface
import Game.Edict

data Trace = Trace { traceAllSolid   :: Bool
                   , traceStartSolid :: Bool
                   , traceFraction   :: Float
                   , traceEndPos     :: V3 Float
                   , tracePlane      :: CPlane
                   , traceSurface    :: CSurface
                   , traceContents   :: Int
                   , traceEnt        :: Edict
                   }
