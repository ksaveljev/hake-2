{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveStateT
  ( module Game.PMoveStateT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

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