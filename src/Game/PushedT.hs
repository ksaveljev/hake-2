{-# LANGUAGE TemplateHaskell #-}
module Game.PushedT
  ( module Game.PushedT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''PushedT

newPushedT :: PushedT
newPushedT =
  PushedT { _pEnt      = Nothing
          , _pOrigin   = V3 0 0 0
          , _pAngles   = V3 0 0 0
          , _pDeltaYaw = 0
          }