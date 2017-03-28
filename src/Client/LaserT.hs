{-# LANGUAGE TemplateHaskell #-}
module Client.LaserT ( LaserT(..)
                     , module Client.LaserT
                     , module Client.EntityT
                     ) where

import Control.Lens (makeLenses)
import Data.IORef (newIORef)
import System.IO.Unsafe (unsafePerformIO)

import Internal
import Client.EntityT

makeLenses ''LaserT

newLaserT :: LaserT
newLaserT =
  LaserT { _lEnt     = unsafePerformIO $ newIORef newEntityT
         , _lEndTime = 0
         }
