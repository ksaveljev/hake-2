{-# LANGUAGE TemplateHaskell #-}
module Game.SpawnTempT
  ( module Game.SpawnTempT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''SpawnTempT

newSpawnTempT :: SpawnTempT
newSpawnTempT =
  SpawnTempT { _stSky       = ""
             , _stSkyRotate = 0
             , _stSkyAxis   = V3 0 0 0
             , _stNextMap   = ""
             , _stLip       = 0
             , _stDistance  = 0
             , _stHeight    = 0
             , _stNoise     = Just ""
             , _stPauseTime = 0
             , _stItem      = Just ""
             , _stGravity   = Just ""
             , _stMinYaw    = 0
             , _stMaxYaw    = 0
             , _stMinPitch  = 0
             , _stMaxPitch  = 0
             }