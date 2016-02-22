{-# LANGUAGE TemplateHaskell #-}
module Game.GameBaseGlobals
  ( module Game.GameBaseGlobals
  ) where

import qualified Constants
import           Game.CPlaneT (newCPlaneT)
import qualified Game.GameItems as GameItems
import           Game.GameLocalsT (newGameLocalsT)
import           Game.LevelLocalsT (newLevelLocalsT)
import           Game.PushedT (newPushedT)
import           Game.SpawnTempT (newSpawnTempT)
import           {-# SOURCE #-} Game.GameImportT (newGameImportT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V
import           Linear (V3(..))

makeLenses ''GameBaseGlobals

initialGameBaseGlobals :: GameBaseGlobals
initialGameBaseGlobals =
  GameBaseGlobals { _gbDummyPlane    = newCPlaneT
                  , _gbGame          = newGameLocalsT
                  , _gbLevel         = newLevelLocalsT
                  , _gbGameImport    = newGameImportT
                  , _gbSpawnTemp     = newSpawnTempT
                  , _gbSmMeatIndex   = 0
                  , _gbSndFry        = 0
                  , _gbMeansOfDeath  = 0
                  , _gbNumEdicts     = 0
                  , _gbItemList      = GameItems.itemList
                  , _gbPushed        = V.replicate Constants.maxEdicts newPushedT
                  , _gbPushedP       = 0
                  , _gbObstacle      = Nothing
                  , _gbCYes          = 0
                  , _gbCNo           = 0
                  , _gbTouch         = V.replicate Constants.maxEdicts (EdictRef (-1))
                  , _gbIsQuad        = False
                  , _gbIsSilenced    = 0
                  , _gbCurrentPlayer = Nothing
                  , _gbCurrentClient = Nothing
                  , _gbForward       = V3 0 0 0
                  , _gbRight         = V3 0 0 0
                  , _gbUp            = V3 0 0 0
                  , _gbXYSpeed       = 0
                  , _gbBobMove       = 0
                  , _gbBobCycle      = 0
                  , _gbBobFracSin    = 0
                  , _gbXxxi          = 0
                  , _gbEnemyVis      = False
                  , _gbEnemyInFront  = False
                  , _gbEnemyRange    = 0
                  , _gbEnemyYaw      = 0
                  , _gbPlayerDieIdx  = 0
                  , _gbWindSound     = 0
                  }