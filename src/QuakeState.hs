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

data QuakeState = QuakeState { _globals    :: Globals
                             , _comGlobals :: ComGlobals
                             , _cmdGlobals :: CmdGlobals
                             }

makeLenses ''QuakeState

defaultQuakeState :: QuakeState
defaultQuakeState =
  QuakeState { _globals    = defaultGlobals
             , _comGlobals = defaultComGlobals
             , _cmdGlobals = defaultCmdGlobals
             }
