module Game.PlayerState where

import Data.Int (Int16)
import Linear.V3 (V3)
import Linear.V4 (V4)
import qualified Data.Vector.Unboxed as UV

import Game.PMoveState

data PlayerState =
  PlayerState { playerStatePMoveState :: PMoveState
              , playerStateViewAngles :: V3 Float
              , playerStateViewOffset :: V3 Float
              , playerStateKickAngles :: V3 Float
              , playerStateGunAngles  :: V3 Float
              , playerStateGunOffset  :: V3 Float
              , playerStateGunIndex   :: Int
              , playerStateGunFrame   :: Int
              , playerStateBlend      :: V4 Float
              , playerStateFOV        :: Float
              , playerStateRDFlags    :: Int
              , playerStateStats      :: UV.Vector Int16
              , playerStatePrototype  :: PlayerState
              }
