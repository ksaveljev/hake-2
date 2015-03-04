{-# LANGUAGE TemplateHaskell #-}
module Game.LevelLocalsT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Game.EdictT

data LevelLocalsT =
  LevelLocalsT { _llFrameNum             :: Int
               , _llTime                 :: Float
               , _llLevelName            :: B.ByteString
               , _llMapName              :: B.ByteString
               , _llNextMap              :: B.ByteString
               , _llIntermissionTime     :: Float
               , _llChangeMap            :: B.ByteString
               , _llExitIntermission     :: Bool
               , _llIntermissionOrigin   :: V3 Float
               , _llIntermissionAngle    :: V3 Float
               , _llSightClient          :: EdictT
               , _llSightEntity          :: EdictT
               , _llSightEntityFrameNum  :: Int
               , _llSoundEntity          :: EdictT
               , _llSoundEntityFrameNum  :: Int
               , _llSound2Entity         :: EdictT
               , _llSound2EntityFrameNum :: Int
               , _llPicHealth            :: Int
               , _llTotalSecrets         :: Int
               , _llFoundSecrets         :: Int
               , _llTotalGoals           :: Int
               , _llFoundGoals           :: Int
               , _llTotalMonsters        :: Int
               , _llKilledMonsters       :: Int
               , _llCurrentEntity        :: EdictT
               , _llBodyQue              :: Int
               , _llPowerCubes           :: Int
               }

makeLenses ''LevelLocalsT
