module Game.ClientRespawn where

import Linear.V3 (V3)

import Game.ClientPersistant

data ClientRespawn = ClientRespawn { clientRespawnCoopRespawn :: ClientPersistant
                                   , clientRespawnEnterFrame  :: Int
                                   , clientRespawnScore       :: Int
                                   , clientRespawnCmdAngles   :: V3 Float
                                   , clientRespawnSpectator   :: Bool
                                   }
