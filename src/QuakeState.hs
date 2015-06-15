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
                  , mBerserkGlobals
                  , mSoldierGlobals
                  , mInfantryGlobals
                  , mBoss2Globals
                  , mBoss31Globals
                  , mBoss32Globals
                  , mBrainGlobals
                  , mChickGlobals
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
                  , ModelExtra(..)
                  , EntThinkAdapter(..)
                  , EntBlockedAdapter(..)
                  , EntTouchAdapter(..)
                  , EntUseAdapter(..)
                  , EntInteractAdapter(..)
                  , AIAdapter(..)
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
                  , module Game.Monsters.MBerserkGlobals
                  , module Game.Monsters.MBoss2Globals
                  , module Game.Monsters.MBoss31Globals
                  , module Game.Monsters.MBoss32Globals
                  , module Game.Monsters.MBrainGlobals
                  , module Game.Monsters.MChickGlobals
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
import Client.ClientGlobals
import Client.CLTEntGlobals
import Client.KeyGlobals
import Client.MenuGlobals
import Client.ParticleTGlobals
import Client.SCRGlobals
import Client.VIDGlobals
import Client.VGlobals
import Game.CmdGlobals
import Game.GameBaseGlobals
import Game.GameItemsGlobals
import Game.Monsters.MBerserkGlobals
import Game.Monsters.MBoss2Globals
import Game.Monsters.MBoss31Globals
import Game.Monsters.MBoss32Globals
import Game.Monsters.MBrainGlobals
import Game.Monsters.MChickGlobals
import Game.Monsters.MInfantryGlobals
import Game.Monsters.MSoldierGlobals
import Game.PlayerTrailGlobals
import QCommon.CMGlobals
import QCommon.ComGlobals
import QCommon.FSGlobals
import QCommon.NetChannelGlobals
import QCommon.PMoveGlobals
import Render.Basic.BasicRenderAPIGlobals
import Render.Fast.FastRenderAPIGlobals
import Render.GLFWbGlobals
import Server.SVGlobals
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
             , _mBerserkGlobals       = initialMBerserkGlobals
             , _mSoldierGlobals       = initialMSoldierGlobals
             , _mInfantryGlobals      = initialMInfantryGlobals
             , _mBoss2Globals         = initialMBoss2Globals
             , _mBoss31Globals        = initialMBoss31Globals
             , _mBoss32Globals        = initialMBoss32Globals
             , _mBrainGlobals         = initialMBrainGlobals
             , _mChickGlobals         = initialMChickGlobals
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
