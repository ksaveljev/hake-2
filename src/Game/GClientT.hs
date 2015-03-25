{-# LANGUAGE TemplateHaskell #-}
module Game.GClientT ( GClientT(..)
                     , module Game.GClientT
                     , module Game.PlayerStateT
                     ) where

import Control.Lens (makeLenses)

import Internal
import Game.PlayerStateT

makeLenses ''GClientT

newGClientT :: GClientT
newGClientT = undefined -- TODO
