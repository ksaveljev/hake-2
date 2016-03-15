{-# LANGUAGE TemplateHaskell #-}
module Game.SpawnTempT
  ( module Game.SpawnTempT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B
import           Linear (V3(..))

makeLenses ''SpawnTempT

newSpawnTempT :: SpawnTempT
newSpawnTempT =
  SpawnTempT { _stSky       = B.empty
             , _stSkyRotate = 0
             , _stSkyAxis   = V3 0 0 0
             , _stNextMap   = B.empty
             , _stLip       = 0
             , _stDistance  = 0
             , _stHeight    = 0
             , _stNoise     = Just B.empty
             , _stPauseTime = 0
             , _stItem      = Just B.empty
             , _stGravity   = Just B.empty
             , _stMinYaw    = 0
             , _stMaxYaw    = 0
             , _stMinPitch  = 0
             , _stMaxPitch  = 0
             }