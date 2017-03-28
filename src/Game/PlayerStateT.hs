{-# LANGUAGE TemplateHaskell #-}
module Game.PlayerStateT ( module Game.PlayerStateT
                         , module Game.PMoveStateT
                         ) where

import Data.Int (Int16)
import Linear (V3(..), V4(..))
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.PMoveStateT
import qualified Constants

data PlayerStateT =
  PlayerStateT { _psPMoveState :: PMoveStateT
               , _psViewAngles :: V3 Float
               , _psViewOffset :: V3 Float
               , _psKickAngles :: V3 Float
               , _psGunAngles  :: V3 Float
               , _psGunOffset  :: V3 Float
               , _psGunIndex   :: Int
               , _psGunFrame   :: Int
               , _psBlend      :: V4 Float
               , _psFOV        :: Float
               , _psRDFlags    :: Int
               , _psStats      :: UV.Vector Int16
               }

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
