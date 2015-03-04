module Game.PMoveStateT where

import Data.Int (Int8, Int16)
import Linear.V3 (V3)

data PMoveStateT =
  PMoveStateT { pmsPMType      :: Int
              , pmsOrigin      :: V3 Float
              , pmsVelocity    :: V3 Float
              , pmsPMFlags     :: Int8
              , pmsPMTime      :: Int8
              , pmsGravity     :: Int16
              , pmsDeltaAngles :: V3 Int16
              , pmsPrototype   :: PMoveStateT
              }
