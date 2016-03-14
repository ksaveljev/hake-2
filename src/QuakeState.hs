{-# LANGUAGE TemplateHaskell #-}
module QuakeState
  ( QuakeState
  , module QuakeState
  , module X )
  where

import Client.ClientGlobals as X
import Client.KeyGlobals as X
import Client.MenuGlobals as X
import Client.ParticleTGlobals as X
import Client.VIDGlobals as X
import Game.CmdGlobals as X
import Game.GameBaseGlobals as X
import {-# SOURCE #-} Game.GameImportT as X
import Game.GameItemsGlobals as X
import Globals as X
import QCommon.CMGlobals as X
import QCommon.ComGlobals as X
import QCommon.FSGlobals as X
import QCommon.PMoveGlobals as X
import Render.Fast.FastRenderAPIGlobals as X
import Render.GLFWbGlobals as X
import Server.SVGlobals as X
import Sys.INGlobals as X
import Sys.KBDGlobals as X
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

worldRef :: Ref EdictT
worldRef = Ref 0

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals              = initialGlobals
             , _comGlobals           = initialComGlobals
             , _cmdGlobals           = initialCmdGlobals
             , _keyGlobals           = initialKeyGlobals
             , _fsGlobals            = initialFSGlobals
             , _svGlobals            = initialSVGlobals
             , _gameBaseGlobals      = initialGameBaseGlobals
             , _cmGlobals            = initialCMGlobals
             , _gameItemsGlobals     = initialGameItemsGlobals
             , _vidGlobals           = initialVIDGlobals
             , _inGlobals            = initialINGlobals
             , _fastRenderAPIGlobals = initialFastRenderAPIGlobals
             , _glfwbGlobals         = initialGLFWbGlobals
             , _clientGlobals        = initialClientGlobals
             , _particleTGlobals     = initialParticleTGlobals
             , _menuGlobals          = initialMenuGlobals
             , _pMoveGlobals         = initialPMoveGlobals
             , _kbdGlobals           = initialKBDGlobals
             }
