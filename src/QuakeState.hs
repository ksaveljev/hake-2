{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , defaultQuakeState
                  , globals
                  , comGlobals
                  , module Globals
                  , module QCommon.ComGlobals
                  ) where

import Control.Lens (makeLenses)

import Globals
import QCommon.ComGlobals

data QuakeState = QuakeState { _globals    :: Globals
                             , _comGlobals :: ComGlobals
                             }

makeLenses ''QuakeState

defaultQuakeState :: QuakeState
defaultQuakeState =
  QuakeState { _globals    = defaultGlobals
             , _comGlobals = defaultComGlobals
             }
