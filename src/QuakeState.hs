{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , initialQuakeState
                  , globals
                  , comGlobals
                  , cmdGlobals
                  , keyGlobals
                  , module Globals
                  , module QCommon.ComGlobals
                  , module Game.CmdGlobals
                  , module Client.KeyGlobals
                  ) where

import Control.Lens (makeLenses)

import Globals
import QCommon.ComGlobals
import Game.CmdGlobals
import Client.KeyGlobals

import Internal

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals    = initialGlobals
             , _comGlobals = initialComGlobals
             , _cmdGlobals = initialCmdGlobals
             , _keyGlobals = initialKeyGlobals
             }
