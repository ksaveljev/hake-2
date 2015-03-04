{-# LANGUAGE TemplateHaskell #-}
module Game.SpawnTempT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data SpawnTempT =
  SpawnTempT { _stSky       :: B.ByteString
             , _stSkyRotate :: Float
             , _stSkyAxis   :: V3 Float
             , _stMap       :: B.ByteString
             , _stLip       :: Int
             , _stDistance  :: Int
             , _stHeight    :: Int
             , _stNoise     :: B.ByteString
             , _stPauseTime :: Float
             , _stItem      :: B.ByteString
             , _stGravity   :: B.ByteString
             , _stMinYaw    :: Float
             , _stMaxYaw    :: Float
             , _stMinPitch  :: Float
             , _stMaxPitch  :: Float
             }

makeLenses ''SpawnTempT
