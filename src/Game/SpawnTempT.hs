module Game.SpawnTempT where

import Linear.V3 (V3)
import qualified Data.ByteString as B

data SpawnTempT =
  SpawnTempT { stSky       :: B.ByteString
             , stSkyRotate :: Float
             , stSkyAxis   :: V3 Float
             , stMap       :: B.ByteString
             , stLip       :: Int
             , stDistance  :: Int
             , stHeight    :: Int
             , stNoise     :: B.ByteString
             , stPauseTime :: Float
             , stItem      :: B.ByteString
             , stGravity   :: B.ByteString
             , stMinYaw    :: Float
             , stMaxYaw    :: Float
             , stMinPitch  :: Float
             , stMaxPitch  :: Float
             }
