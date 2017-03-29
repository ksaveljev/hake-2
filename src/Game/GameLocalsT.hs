{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.GameLocalsT ( GameLocalsT(..)
                        , module Game.GameLocalsT
                        ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Types
import Game.GClientT
import qualified Constants

makeLenses ''GameLocalsT

newGameLocalsT :: GameLocalsT
newGameLocalsT =
  GameLocalsT { _glHelpMessage1 = ""
              , _glHelpMessage2 = ""
              , _glHelpChanged  = 0
              , _glClients      = V.generate Constants.maxClients newGClientT
              , _glSpawnPoint   = ""
              , _glMaxClients   = 0
              , _glMaxEntities  = 0
              , _glServerFlags  = 0
              , _glNumItems     = 0
              , _glAutosaved    = False
              }
