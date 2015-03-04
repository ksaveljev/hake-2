{-# LANGUAGE TemplateHaskell #-}
module Game.ClientRespawnT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.ClientPersistantT

data ClientRespawnT =
  ClientRespawnT { _crCoopRespawn :: ClientPersistantT
                 , _crEnterFrame  :: Int
                 , _crScore       :: Int
                 , _crCmdAngles   :: V3 Float
                 , _crSpectator   :: Bool
                 }

makeLenses ''ClientRespawnT
