{-# LANGUAGE TemplateHaskell #-}
module Client.CEntityT where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Internal
import Game.EntityStateT

makeLenses ''CEntityT

newCEntityT :: CEntityT
newCEntityT =
  CEntityT { _ceBaseline    = newEntityStateT
           , _ceCurrent     = newEntityStateT
           , _cePrev        = newEntityStateT
           , _ceServerFrame = 0
           , _ceTrailCount  = 0
           , _ceLerpOrigin  = V3 0 0 0
           , _ceFlyStopTime = 0
           }
