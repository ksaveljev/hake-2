{-# LANGUAGE TemplateHaskell #-}
module Game.PMoveStateT where

import           Control.Lens (makeLenses)
import           Data.Int     (Int8, Int16)
import           Linear.V3    (V3(..))

import           Types

makeLenses ''PMoveStateT

newPMoveStateT :: PMoveStateT
newPMoveStateT = PMoveStateT
    { _pmsPMType      = 0
    , _pmsOrigin      = V3 0 0 0
    , _pmsVelocity    = V3 0 0 0
    , _pmsPMFlags     = 0
    , _pmsPMTime      = 0
    , _pmsGravity     = 0
    , _pmsDeltaAngles = V3 0 0 0
    }