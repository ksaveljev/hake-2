module Game.PlayerStateT where

import Data.Int (Int16)
import Linear.V3 (V3)
import Linear.V4 (V4)
import qualified Data.Vector.Unboxed as UV

import Game.PMoveStateT

data PlayerStateT =
  PlayerStateT { psPMoveState :: PMoveStateT
               , psViewAngles :: V3 Float
               , psViewOffset :: V3 Float
               , psKickAngles :: V3 Float
               , psGunAngles  :: V3 Float
               , psGunOffset  :: V3 Float
               , psGunIndex   :: Int
               , psGunFrame   :: Int
               , psBlend      :: V4 Float
               , psFOV        :: Float
               , psRDFlags    :: Int
               , psStats      :: UV.Vector Int16
               , psPrototype  :: PlayerStateT
               }
