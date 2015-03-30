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
                  , gameBaseGlobals
                  , pMoveGlobals
                  , scrGlobals
                  , netGlobals
                  , cmGlobals
                  , EdictIndex(..)
                  , ClientIndex(..)
                  , GClientIndex(..)
                  , module Globals
                  , module QCommon.CMGlobals
                  , module QCommon.ComGlobals
                  , module Game.CmdGlobals
                  , module Client.KeyGlobals
                  , module Client.SCRGlobals
                  , module QCommon.FSGlobals
                  , module QCommon.NetChannelGlobals
                  , module Server.SVGlobals
                  , module Game.GameBaseGlobals
                  , module QCommon.PMoveGlobals
                  , module Sys.NETGlobals
                  ) where

import Control.Lens (makeLenses)

import Internal
import Globals
import Game.CmdGlobals
import Game.GameBaseGlobals
import Client.KeyGlobals
import Client.SCRGlobals
import Server.SVGlobals
import QCommon.CMGlobals
import QCommon.ComGlobals
import QCommon.FSGlobals
import QCommon.NetChannelGlobals
import QCommon.PMoveGlobals
import Sys.NETGlobals

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
             , _gameBaseGlobals   = initialGameBaseGlobals
             , _pMoveGlobals      = initialPMoveGlobals
             , _scrGlobals        = initialSCRGlobals
             , _netGlobals        = initialNETGlobals
             , _cmGlobals         = initialCMGlobals
             }
