{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveStateT where

import Data.Int (Int8, Int16)
import Linear.V3 (V3(..))
import Control.Lens (makeLenses)

data PMoveStateT =
  PMoveStateT { _pmsPMType      :: Int
              , _pmsOrigin      :: V3 Float
              , _pmsVelocity    :: V3 Float
              , _pmsPMFlags     :: Int8
              , _pmsPMTime      :: Int8
              , _pmsGravity     :: Int16
              , _pmsDeltaAngles :: V3 Int16
              }

makeLenses ''PMoveStateT

newPMoveStateT :: PMoveStateT
newPMoveStateT =
  PMoveStateT { _pmsPMType      = 0
              , _pmsOrigin      = V3 0 0 0
              , _pmsVelocity    = V3 0 0 0
              , _pmsPMFlags     = 0
              , _pmsPMTime      = 0
              , _pmsGravity     = 0
              , _pmsDeltaAngles = V3 0 0 0
              }
