module Game.PMoveState where

import Data.Int (Int8, Int16)
import Linear.V3 (V3)

data PMoveState =
  PMoveState { pMoveStatePMType      :: Int
             , pMoveStateOrigin      :: V3 Float
             , pMoveStateVelocity    :: V3 Float
             , pMoveStatePMFlags     :: Int8
             , pMoveStatePMTime      :: Int8
             , pMoveStateGravity     :: Int16
             , pMoveStateDeltaAngles :: V3 Int16
             , pMoveStatePrototype   :: PMoveState
             }
