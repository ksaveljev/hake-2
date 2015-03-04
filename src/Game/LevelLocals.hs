module Game.LevelLocals where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Game.Edict

data LevelLocals = LevelLocals { levelLocalsFrameNum             :: Int
                               , levelLocalsTime                 :: Float
                               , levelLocalsLevelName            :: B.ByteString
                               , levelLocalsMapName              :: B.ByteString
                               , levelLocalsNextMap              :: B.ByteString
                               , levelLocalsIntermissionTime     :: Float
                               , levelLocalsChangeMap            :: B.ByteString
                               , levelLocalsExitIntermission     :: Bool
                               , levelLocalsIntermissionOrigin   :: V3 Float
                               , levelLocalsIntermissionAngle    :: V3 Float
                               , levelLocalsSightClient          :: Edict
                               , levelLocalsSightEntity          :: Edict
                               , levelLocalsSightEntityFrameNum  :: Int
                               , levelLocalsSoundEntity          :: Edict
                               , levelLocalsSoundEntityFrameNum  :: Int
                               , levelLocalsSound2Entity         :: Edict
                               , levelLocalsSound2EntityFrameNum :: Int
                               , levelLocalsPicHealth            :: Int
                               , levelLocalsTotalSecrets         :: Int
                               , levelLocalsFoundSecrets         :: Int
                               , levelLocalsTotalGoals           :: Int
                               , levelLocalsFoundGoals           :: Int
                               , levelLocalsTotalMonsters        :: Int
                               , levelLocalsKilledMonsters       :: Int
                               , levelLocalsCurrentEntity        :: Edict
                               , levelLocalsBodyQue              :: Int
                               , levelLocalsPowerCubes           :: Int
                               }
