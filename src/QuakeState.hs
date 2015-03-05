{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , initialQuakeState
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

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals    = initialGlobals
             , _comGlobals = initialComGlobals
             , _cmdGlobals = initialCmdGlobals
             }
