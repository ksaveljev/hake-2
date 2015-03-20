{-# LANGUAGE TemplateHaskell #-}
module Game.GameLocalsT ( GameLocalsT(..)
                        , module Game.GameLocalsT
                        ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''GameLocalsT

newGameLocalsT :: GameLocalsT
newGameLocalsT = undefined -- TODO
