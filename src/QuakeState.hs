{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , initialQuakeState
                  , globals
                  , comGlobals
                  , cmdGlobals
                  , keyGlobals
                  , cvarGlobals
                  , fsGlobals
                  , netChannelGlobals
                  , SizeBufTLens
                  , module Globals
                  , module CVarGlobals
                  , module QCommon.ComGlobals
                  , module Game.CmdGlobals
                  , module Client.KeyGlobals
                  , module QCommon.FSGlobals
                  , module QCommon.NetChannelGlobals
                  ) where

import Control.Lens (makeLenses)

import Globals
import CVarGlobals
import QCommon.ComGlobals
import Game.CmdGlobals
import Client.KeyGlobals
import QCommon.FSGlobals
import QCommon.NetChannelGlobals

import Internal

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals           = initialGlobals
             , _comGlobals        = initialComGlobals
             , _cmdGlobals        = initialCmdGlobals
             , _keyGlobals        = initialKeyGlobals
             , _cvarGlobals       = initialCVarGlobals
             , _fsGlobals         = initialFSGlobals
             , _netChannelGlobals = initialNetChannelGlobals
             }
