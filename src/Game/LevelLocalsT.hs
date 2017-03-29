{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.LevelLocalsT ( LevelLocalsT(..)
                         , module Game.LevelLocalsT
                         ) where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Types

makeLenses ''LevelLocalsT

newLevelLocalsT :: LevelLocalsT
newLevelLocalsT =
  LevelLocalsT { _llFrameNum             = 0
               , _llTime                 = 0
               , _llLevelName            = ""
               , _llMapName              = ""
               , _llNextMap              = ""
               , _llIntermissionTime     = 0
               , _llChangeMap            = ""
               , _llExitIntermission     = False
               , _llIntermissionOrigin   = V3 0 0 0
               , _llIntermissionAngle    = V3 0 0 0
               , _llSightClient          = Nothing
               , _llSightEntity          = Nothing
               , _llSightEntityFrameNum  = 0
               , _llSoundEntity          = Nothing
               , _llSoundEntityFrameNum  = 0
               , _llSound2Entity         = Nothing
               , _llSound2EntityFrameNum = 0
               , _llPicHealth            = 0
               , _llTotalSecrets         = 0
               , _llFoundSecrets         = 0
               , _llTotalGoals           = 0
               , _llFoundGoals           = 0
               , _llTotalMonsters        = 0
               , _llKilledMonsters       = 0
               , _llCurrentEntity        = Nothing
               , _llBodyQue              = 0
               , _llPowerCubes           = 0
               }
