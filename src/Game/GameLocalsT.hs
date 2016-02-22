{-# LANGUAGE TemplateHaskell #-}
module Game.GameLocalsT
  ( module Game.GameLocalsT
  ) where

import qualified Constants
import           Game.GClientT (newGClientT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

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