{-# LANGUAGE TemplateHaskell #-}
module Game.PlayerStateT where

import Data.Int (Int16)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.PMoveStateT

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
               , _psPrototype  :: PlayerStateT
               }

makeLenses ''PlayerStateT
