{-# LANGUAGE TemplateHaskell #-}
module Game.GItemT ( GItemT(..)
                   , module Game.GItemT
                   , module Game.GItemArmorT
                   ) where

import Control.Lens (makeLenses)

import Internal
import Game.GItemArmorT

makeLenses ''GItemT
