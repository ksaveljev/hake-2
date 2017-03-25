{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT
    ( module Client.LaserT
    ) where

import           Control.Lens     (makeLenses)
import           Data.IORef       (newIORef)
import           System.IO.Unsafe (unsafePerformIO)

import           Client.EntityT   (newEntityT)
import           Types

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT = LaserT
    { _lEnt     = unsafePerformIO (newIORef newEntityT)
    , _lEndTime = 0
    }