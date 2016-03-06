{-# LANGUAGE TemplateHaskell #-}
module QuakeState
  ( QuakeState
  , module QuakeState
  , module X )
  where

import Client.KeyGlobals as X
import Client.VIDGlobals as X
import Game.CmdGlobals as X
import Game.GameBaseGlobals as X
import {-# SOURCE #-} Game.GameImportT as X
import Game.GameItemsGlobals as X
import Globals as X
import QCommon.CMGlobals as X
import QCommon.ComGlobals as X
import QCommon.FSGlobals as X
import Server.SVGlobals as X
import Sys.INGlobals as X
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals          = initialGlobals
             , _comGlobals       = initialComGlobals
             , _cmdGlobals       = initialCmdGlobals
             , _keyGlobals       = initialKeyGlobals
             , _fsGlobals        = initialFSGlobals
             , _svGlobals        = initialSVGlobals
             , _gameBaseGlobals  = initialGameBaseGlobals
             , _cmGlobals        = initialCMGlobals
             , _gameItemsGlobals = initialGameItemsGlobals
             , _vidGlobals       = initialVIDGlobals
             , _inGlobals        = initialINGlobals
             }
