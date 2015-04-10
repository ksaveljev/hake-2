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
import qualified Data.Vector as V

import Internal
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
                  , _gbGEdicts           = V.generate Constants.maxEdicts newEdictT
                  , _gbItemList          = GameItemList.itemList
                  , _gbPushed            = V.replicate Constants.maxEdicts newPushedT
                  , _gbPushedP           = 0
                  , _gbObstacle          = Nothing
                  , _gbCYes              = 0
                  , _gbCNo               = 0
                  , _gbTouch             = V.replicate Constants.maxEdicts (EdictReference (-1))
                  }
