module Game.SpawnTemp where

import Linear.V3 (V3)
import qualified Data.ByteString as B

data SpawnTemp = SpawnTemp { spawnTempSky       :: B.ByteString
                           , spawnTempSkyRotate :: Float
                           , spawnTempSkyAxis   :: V3 Float
                           , spawnNextMap       :: B.ByteString
                           , spawnTempLip       :: Int
                           , spawnTempDistance  :: Int
                           , spawnTempHeight    :: Int
                           , spawnTempNoise     :: B.ByteString
                           , spawnTempPauseTime :: Float
                           , spawnTempItem      :: B.ByteString
                           , spawnTempGravity   :: B.ByteString
                           , spawnTempMinYaw    :: Float
                           , spawnTempMaxYaw    :: Float
                           , spawnTempMinPitch  :: Float
                           , spawnTempMaxPitch  :: Float
                           }
