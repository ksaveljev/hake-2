{-# LANGUAGE TemplateHaskell #-}
module Game.PlayerStateT
  ( module Game.PlayerStateT
  ) where

import qualified Constants
import           Game.PMoveStateT (newPMoveStateT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..), V4(..))

makeLenses ''PlayerStateT

newPlayerStateT :: PlayerStateT
newPlayerStateT =
  PlayerStateT { _psPMoveState = newPMoveStateT
               , _psViewAngles = V3 0 0 0
               , _psViewOffset = V3 0 0 0
               , _psKickAngles = V3 0 0 0
               , _psGunAngles  = V3 0 0 0
               , _psGunOffset  = V3 0 0 0
               , _psGunIndex   = 0
               , _psGunFrame   = 0
               , _psBlend      = V4 0 0 0 0
               , _psFOV        = 0
               , _psRDFlags    = 0
               , _psStats      = UV.replicate Constants.maxStats 0
               }