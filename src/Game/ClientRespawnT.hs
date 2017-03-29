{-# LANGUAGE TemplateHaskell #-}
module Game.ClientRespawnT where

import           Control.Lens (makeLenses)
import           Linear       (V3(..))

import           Game.ClientPersistantT
import           Types

makeLenses ''ClientRespawnT

newClientRespawnT :: ClientRespawnT
newClientRespawnT = ClientRespawnT
    { _crCoopRespawn = newClientPersistantT
    , _crEnterFrame  = 0
    , _crScore       = 0
    , _crCmdAngles   = V3 0 0 0
    , _crSpectator   = False
    }
