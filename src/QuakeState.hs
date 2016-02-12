{-# LANGUAGE TemplateHaskell #-}
module QuakeState
  (module QuakeState
  ,module Game.CVarT
  ,QuakeState)
  where

import Game.CVarT
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState = undefined
