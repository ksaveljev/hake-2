{-# LANGUAGE TemplateHaskell #-}
module Game.GItemT ( GItemT(..)
                   , module Game.GItemT
                   , module Game.GItemArmorT
                   ) where

import Control.Lens (makeLenses)

import Types
import Game.GItemArmorT

makeLenses ''GItemT
