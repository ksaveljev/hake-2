{-# LANGUAGE TemplateHaskell #-}
module Game.GClientT where

import Control.Lens (makeLenses)

import Game.Internal

makeLenses ''GClientT
