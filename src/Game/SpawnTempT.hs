{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.SpawnTempT where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.ByteString as B

data SpawnTempT =
  SpawnTempT { _stSky       :: B.ByteString
             , _stSkyRotate :: Float
             , _stSkyAxis   :: V3 Float
             , _stNextMap   :: B.ByteString
             , _stLip       :: Int
             , _stDistance  :: Int
             , _stHeight    :: Int
             , _stNoise     :: Maybe B.ByteString
             , _stPauseTime :: Float
             , _stItem      :: Maybe B.ByteString
             , _stGravity   :: Maybe B.ByteString
             , _stMinYaw    :: Float
             , _stMaxYaw    :: Float
             , _stMinPitch  :: Float
             , _stMaxPitch  :: Float
             }

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
