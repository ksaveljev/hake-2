{-# LANGUAGE TemplateHaskell #-}
module Client.CEntityT ( CEntityT(..)
                       , module Client.CEntityT
                       ) where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Types
import Game.EntityStateT

makeLenses ''CEntityT

newCEntityT :: CEntityT
newCEntityT =
  CEntityT { _ceBaseline    = newEntityStateT Nothing
           , _ceCurrent     = newEntityStateT Nothing
           , _cePrev        = newEntityStateT Nothing
           , _ceServerFrame = 0
           , _ceTrailCount  = 0
           , _ceLerpOrigin  = V3 0 0 0
           , _ceFlyStopTime = 0
           }
