{-# LANGUAGE TemplateHaskell #-}
module QuakeState
  (QuakeState
  ,module QuakeState
  ,module X)
  where

import Game.CVarT as X
import Globals as X
import QCommon.ComGlobals as X
import QCommon.SizeBufT as X
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals    = initialGlobals
             , _comGlobals = initialComGlobals
             }
