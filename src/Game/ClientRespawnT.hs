module Game.ClientRespawnT where

import Linear.V3 (V3)

import Game.ClientPersistantT

data ClientRespawnT =
  ClientRespawnT { crCoopRespawn :: ClientPersistantT
                 , crEnterFrame  :: Int
                 , crScore       :: Int
                 , crCmdAngles   :: V3 Float
                 , crSpectator   :: Bool
                 }
