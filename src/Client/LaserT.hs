{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT
  ( module Client.LaserT
  ) where

import Client.EntityT (newEntityT)
import Types

import Control.Lens (makeLenses)

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT =
  LaserT { _lEnt     = newEntityT
         , _lEndTime = 0
         }