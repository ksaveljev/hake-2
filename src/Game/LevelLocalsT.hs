module Game.LevelLocalsT where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Game.EdictT

data LevelLocalsT =
  LevelLocalsT { llFrameNum             :: Int
               , llTime                 :: Float
               , llLevelName            :: B.ByteString
               , llMapName              :: B.ByteString
               , llNextMap              :: B.ByteString
               , llIntermissionTime     :: Float
               , llChangeMap            :: B.ByteString
               , llExitIntermission     :: Bool
               , llIntermissionOrigin   :: V3 Float
               , llIntermissionAngle    :: V3 Float
               , llSightClient          :: EdictT
               , llSightEntity          :: EdictT
               , llSightEntityFrameNum  :: Int
               , llSoundEntity          :: EdictT
               , llSoundEntityFrameNum  :: Int
               , llSound2Entity         :: EdictT
               , llSound2EntityFrameNum :: Int
               , llPicHealth            :: Int
               , llTotalSecrets         :: Int
               , llFoundSecrets         :: Int
               , llTotalGoals           :: Int
               , llFoundGoals           :: Int
               , llTotalMonsters        :: Int
               , llKilledMonsters       :: Int
               , llCurrentEntity        :: EdictT
               , llBodyQue              :: Int
               , llPowerCubes           :: Int
               }
