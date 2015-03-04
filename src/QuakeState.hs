{-# LANGUAGE TemplateHaskell #-}
module QuakeState where

import Control.Lens (makeLenses)

import Globals

data QuakeState = QuakeState { _globals :: Globals
                             }

makeLenses ''QuakeState
