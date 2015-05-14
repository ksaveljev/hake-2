{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT where

import Control.Lens (makeLenses)

import Internal

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT =
  LaserT { _lEnt     = newEntityT
         , _lEndTime = 0
         }
