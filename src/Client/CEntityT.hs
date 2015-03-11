{-# LANGUAGE TemplateHaskell #-}
module Client.CEntityT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.EntityStateT

data CEntityT =
  CEntityT { _ceBaseline    :: EntityStateT
           , _ceCurrent     :: EntityStateT
           , _cePrev        :: EntityStateT
           , _ceServerFrame :: Int
           , _ceTrailCount  :: Int
           , _ceLerpOrigin  :: V3 Float
           , _ceFlyStopTime :: Int
           }

makeLenses ''CEntityT
