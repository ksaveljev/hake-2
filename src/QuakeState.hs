{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , defaultQuakeState
                  , globals
                  , module Globals
                  ) where

import Control.Lens (makeLenses)

import Globals

data QuakeState = QuakeState { _globals :: Globals
                             }

makeLenses ''QuakeState

defaultQuakeState :: QuakeState
defaultQuakeState =
  QuakeState { _globals = defaultGlobals
             }
