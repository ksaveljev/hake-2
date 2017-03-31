{-# LANGUAGE TemplateHaskell #-}
module Game.GameBaseGlobals ( module Game.GameBaseGlobals
                            , module Game.CVarT
                            , module Game.CPlaneT
                            , module Game.GameLocalsT
                            , module Game.GameImportT
                            , module Game.GItemT
                            , module Game.LevelLocalsT
                            , module Game.PushedT
                            , module Game.SpawnTempT
                            ) where

import Control.Lens (makeLenses)
import Linear (V3(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V

import Types
import Game.CVarT
import Game.CPlaneT
import Game.EdictT
import Game.GameLocalsT
import {-# SOURCE #-} Game.GameImportT
import Game.GItemT
import Game.LevelLocalsT
import Game.PushedT
import Game.SpawnTempT
import {-# SOURCE #-} qualified Game.GameItemList as GameItemList
import qualified Constants

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
                  , _gbGEdicts           = unsafePerformIO $ V.thaw $ V.generate (Constants.maxEdicts + 1) newEdictT -- one extra for "dummy edict"
                  , _gbItemList          = GameItemList.itemList
                  , _gbPushed            = V.replicate Constants.maxEdicts newPushedT
                  , _gbPushedP           = 0
                  , _gbObstacle          = Nothing
                  , _gbCYes              = 0
                  , _gbCNo               = 0
                  , _gbTouch             = V.replicate Constants.maxEdicts (Ref (-1))
                  , _gbIsQuad            = False
                  , _gbIsSilenced        = 0
                  , _gbCurrentPlayer     = Nothing
                  , _gbCurrentClient     = Nothing
                  , _gbForward           = V3 0 0 0
                  , _gbRight             = V3 0 0 0
                  , _gbUp                = V3 0 0 0
                  , _gbXYSpeed           = 0
                  , _gbBobMove           = 0
                  , _gbBobCycle          = 0
                  , _gbBobFracSin        = 0
                  , _gbXxxi              = 0
                  , _gbEnemyVis          = False
                  , _gbEnemyInFront      = False
                  , _gbEnemyRange        = 0
                  , _gbEnemyYaw          = 0
                  , _gbPlayerDieIdx      = 0
                  , _gbWindSound         = 0
                  }
