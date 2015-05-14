{-# LANGUAGE TemplateHaskell #-}
module QuakeState ( QuakeState(..)
                  , initialQuakeState
                  , globals
                  , comGlobals
                  , cmdGlobals
                  , keyGlobals
                  , fsGlobals
                  , svGlobals
                  , gameBaseGlobals
                  , pMoveGlobals
                  , scrGlobals
                  , netGlobals
                  , cmGlobals
                  , gameItemsGlobals
                  , mSoldierGlobals
                  , mInfantryGlobals
                  , playerTrailGlobals
                  , vidGlobals
                  , inGlobals
                  , glfwbGlobals
                  , kbdGlobals
                  , basicRenderAPIGlobals
                  , fastRenderAPIGlobals
                  , particleTGlobals
                  , menuGlobals
                  , clientGlobals
                  , vGlobals
                  , netChannelGlobals
                  , clTEntGlobals
                  , EdictReference(..)
                  , ClientReference(..)
                  , GClientReference(..)
                  , CModelReference(..)
                  , LinkReference(..)
                  , GItemReference(..)
                  , ImageReference(..)
                  , UserCmdReference(..)
                  , CParticleReference(..)
                  , ModelReference(..)
                  , GLPolyReference(..)
                  , CPlaneReference(..)
                  , MSurfaceReference(..)
                  , MTexInfoReference(..)
                  , MNodeReference(..)
                  , SfxReference(..)
                  , MNodeChild(..)
                  , EntThinkAdapter(..)
                  , EntBlockedAdapter(..)
                  , EntTouchAdapter(..)
                  , EntUseAdapter(..)
                  , EntInteractAdapter(..)
                  , module Globals
                  , module QCommon.CMGlobals
                  , module QCommon.ComGlobals
                  , module Game.CmdGlobals
                  , module Client.ClientGlobals
                  , module Client.CLTEntGlobals
                  , module Client.KeyGlobals
                  , module Client.MenuGlobals
                  , module Client.ParticleTGlobals
                  , module Client.SCRGlobals
                  , module Client.VGlobals
                  , module QCommon.FSGlobals
                  , module Server.SVGlobals
                  , module Game.GameBaseGlobals
                  , module QCommon.PMoveGlobals
                  , module Sys.KBDGlobals
                  , module Sys.INGlobals
                  , module Sys.NETGlobals
                  , module Game.GameItemsGlobals
                  , module Game.Monsters.MSoldierGlobals
                  , module Game.Monsters.MInfantryGlobals
                  , module Game.PlayerTrailGlobals
                  , module Client.VIDGlobals
                  , module Render.Basic.BasicRenderAPIGlobals
                  , module Render.Fast.FastRenderAPIGlobals
                  , module Render.GLFWbGlobals
                  , module QCommon.NetChannelGlobals
                  ) where

import Control.Lens (makeLenses)

import Internal
import Globals
import Game.CmdGlobals
import Game.GameBaseGlobals
import Game.GameItemsGlobals
import Game.Monsters.MInfantryGlobals
import Game.Monsters.MSoldierGlobals
import Game.PlayerTrailGlobals
import Client.ClientGlobals
import Client.CLTEntGlobals
import Client.KeyGlobals
import Client.MenuGlobals
import Client.ParticleTGlobals
import Client.SCRGlobals
import Client.VIDGlobals
import Client.VGlobals
import Server.SVGlobals
import QCommon.CMGlobals
import QCommon.ComGlobals
import QCommon.FSGlobals
import QCommon.NetChannelGlobals
import QCommon.PMoveGlobals
import Render.Basic.BasicRenderAPIGlobals
import Render.Fast.FastRenderAPIGlobals
import Render.GLFWbGlobals
import Sys.KBDGlobals
import Sys.INGlobals
import Sys.NETGlobals

makeLenses ''QuakeState

initialQuakeState :: QuakeState
initialQuakeState =
  QuakeState { _globals               = initialGlobals
             , _comGlobals            = initialComGlobals
             , _cmdGlobals            = initialCmdGlobals
             , _keyGlobals            = initialKeyGlobals
             , _fsGlobals             = initialFSGlobals
             , _svGlobals             = initialSVGlobals
             , _gameBaseGlobals       = initialGameBaseGlobals
             , _pMoveGlobals          = initialPMoveGlobals
             , _scrGlobals            = initialSCRGlobals
             , _netGlobals            = initialNETGlobals
             , _cmGlobals             = initialCMGlobals
             , _gameItemsGlobals      = initialGameItemsGlobals
             , _mSoldierGlobals       = initialMSoldierGlobals
             , _mInfantryGlobals      = initialMInfantryGlobals
             , _playerTrailGlobals    = initialPlayerTrailGlobals
             , _vidGlobals            = initialVIDGlobals
             , _inGlobals             = initialINGlobals
             , _glfwbGlobals          = initialGLFWbGlobals
             , _kbdGlobals            = initialKBDGlobals
             , _basicRenderAPIGlobals = initialBasicRenderAPIGlobals
             , _fastRenderAPIGlobals  = initialFastRenderAPIGlobals
             , _particleTGlobals      = initialParticleTGlobals
             , _menuGlobals           = initialMenuGlobals
             , _clientGlobals         = initialClientGlobals
             , _vGlobals              = initialVGlobals
             , _netChannelGlobals     = initialNetChannelGlobals
             , _clTEntGlobals         = initialCLTEntGlobals
             }
