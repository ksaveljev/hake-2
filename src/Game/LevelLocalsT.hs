{-# LANGUAGE TemplateHaskell #-}
module Game.LevelLocalsT ( LevelLocalsT(..)
                         , module Game.LevelLocalsT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''LevelLocalsT

newLevelLocalsT :: LevelLocalsT
newLevelLocalsT = undefined -- TODO
