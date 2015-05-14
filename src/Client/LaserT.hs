{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT ( LaserT(..)
                     , module Client.LaserT
                     , module Client.EntityT
                     ) where

import Control.Lens (makeLenses)

import Internal
import Client.EntityT

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT =
  LaserT { _lEnt     = newEntityT
         , _lEndTime = 0
         }
