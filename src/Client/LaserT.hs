{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT where

import Control.Lens (makeLenses)

import Client.EntityT

data LaserT =
  LaserT { _lEnt     :: EntityT
         , _lEndTime :: Int
         }

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT =
  LaserT { _lEnt     = newEntityT
         , _lEndTime = 0
         }
