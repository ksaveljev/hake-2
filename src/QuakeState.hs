{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , defaultQuakeState
                  , globals
                  , comGlobals
                  , cmdGlobals
                  , module Globals
                  , module QCommon.ComGlobals
                  , module Game.CmdGlobals
                  ) where

import Control.Lens (makeLenses)

import Globals
import QCommon.ComGlobals
import Game.CmdGlobals

import Internal

makeLenses ''QuakeState

defaultQuakeState :: QuakeState
defaultQuakeState =
  QuakeState { _globals    = defaultGlobals
             , _comGlobals = defaultComGlobals
             , _cmdGlobals = defaultCmdGlobals
             }
