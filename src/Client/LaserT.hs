{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT where

import           Control.Lens     (makeLenses)
import           Data.IORef       (newIORef)
import           System.IO.Unsafe (unsafePerformIO)

import           Client.EntityT
import           Types

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT = LaserT
    { _lEnt     = unsafePerformIO (newIORef newEntityT)
    , _lEndTime = 0
    }
