{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , initialQuakeState
                  , globals
                  , comGlobals
                  , cmdGlobals
                  , keyGlobals
                  , fsGlobals
                  , netChannelGlobals
                  , svGlobals
                  , SizeBufTLens
                  , CVarTLens
                  , ClientTLens
                  , EdictTLens
                  , module Globals
                  , module QCommon.ComGlobals
                  , module Game.CmdGlobals
                  , module Client.KeyGlobals
                  , module QCommon.FSGlobals
                  , module QCommon.NetChannelGlobals
                  , module Server.SVGlobals
                  ) where

import Control.Lens (makeLenses)

import Internal
import Globals
import Game.CmdGlobals
import Client.KeyGlobals
import Server.SVGlobals
import QCommon.ComGlobals
import QCommon.FSGlobals
import QCommon.NetChannelGlobals

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals           = initialGlobals
             , _comGlobals        = initialComGlobals
             , _cmdGlobals        = initialCmdGlobals
             , _keyGlobals        = initialKeyGlobals
             , _fsGlobals         = initialFSGlobals
             , _netChannelGlobals = initialNetChannelGlobals
             , _svGlobals         = initialSVGlobals
             }
