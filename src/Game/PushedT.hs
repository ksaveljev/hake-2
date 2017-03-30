{-# LANGUAGE TemplateHaskell #-}
module Game.PushedT where

import           Control.Lens (makeLenses)
import           Linear       (V3(..))

import           Types

makeLenses ''PushedT

newPushedT :: PushedT
newPushedT = PushedT
    { _pEnt      = Nothing
    , _pOrigin   = V3 0 0 0
    , _pAngles   = V3 0 0 0
    , _pDeltaYaw = 0
    }