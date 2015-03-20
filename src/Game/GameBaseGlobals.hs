{-# LANGUAGE TemplateHaskell #-}
module Game.GameBaseGlobals ( module Game.GameBaseGlobals
                            , module Game.CVarT
                            , module Game.CPlaneT
                            , module Game.GameLocalsT
                            , module Game.GameImportT
                            , module Game.LevelLocalsT
                            , module Game.SpawnTempT
                            ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Game.CVarT
import Game.CPlaneT
import Game.GameLocalsT
import Game.GameImportT
import Game.LevelLocalsT
import Game.SpawnTempT

makeLenses ''GameBaseGlobals

initialGameBaseGlobals :: GameBaseGlobals
initialGameBaseGlobals =
  GameBaseGlobals { _gbDummyPlane        = newCPlaneT
                  , _gbGame              = newGameLocalsT
                  , _gbLevel             = newLevelLocalsT
                  , _gbGameImport        = newGameImportT
                  , _gbSpawnTemp         = newSpawnTempT
                  , _gbSmMeatIndex       = 0
                  , _gbSndFry            = 0
                  , _gbMeansOfDeath      = 0
                  , _gbNumEdicts         = 0
                  , _gbGEdicts           = V.empty -- TODO
                  , _gbDeathmatch        = newCVarT
                  , _gbCoop              = newCVarT
                  , _gbDMFlags           = newCVarT
                  , _gbSkill             = newCVarT
                  , _gbFragLimit         = newCVarT
                  , _gbTimeLimit         = newCVarT
                  , _gbPassword          = newCVarT
                  , _gbSpectatorPassword = newCVarT
                  , _gbNeedPass          = newCVarT
                  , _gbMaxClients        = newCVarT
                  , _gbMaxSpectators     = newCVarT
                  , _gbMaxEntities       = newCVarT
                  , _gbGSelectEmpty      = newCVarT
                  , _gbFilterBan         = newCVarT
                  , _gbSvMaxVelocity     = newCVarT
                  , _gbSvGravity         = newCVarT
                  , _gbSvRollSpeed       = newCVarT
                  , _gbSvRollAngle       = newCVarT
                  , _gbGunX              = newCVarT
                  , _gbGunY              = newCVarT
                  , _gbGunZ              = newCVarT
                  , _gbRunPitch          = newCVarT
                  , _gbRunRoll           = newCVarT
                  , _gbBobUp             = newCVarT
                  , _gbBobPitch          = newCVarT
                  , _gbBolRoll           = newCVarT
                  , _gbSvCheats          = newCVarT
                  , _gbFloodMsgs         = newCVarT
                  , _gbFloodPerSecond    = newCVarT
                  , _gbFloodWaitDelay    = newCVarT
                  , _gbSvMapList         = newCVarT
                  }
