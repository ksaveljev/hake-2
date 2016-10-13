{-# LANGUAGE TemplateHaskell #-}
module QuakeState
    ( QuakeState
    , module QuakeState
    , module X
    ) where

import Client.ClientGlobals             as X
import Client.CLTEntGlobals             as X
import Client.KeyGlobals                as X
import Client.MenuGlobals               as X
import Client.ParticleTGlobals          as X
import Client.SCRGlobals                as X
import Client.VGlobals                  as X
import Client.VIDGlobals                as X
import Game.CmdGlobals                  as X
import Game.GameBaseGlobals             as X
import {-# SOURCE #-} Game.GameImportT  as X
import Game.GameItemsGlobals            as X
import Game.Monsters.MBerserkGlobals    as X
import Game.Monsters.MBoss2Globals      as X
import Game.Monsters.MBoss31Globals     as X
import Game.Monsters.MBoss32Globals     as X
import Game.Monsters.MBrainGlobals      as X
import Game.Monsters.MChickGlobals      as X
import Game.Monsters.MFlipperGlobals    as X
import Game.Monsters.MFloatGlobals      as X
import Game.Monsters.MFlyerGlobals      as X
import Game.Monsters.MGladiatorGlobals  as X
import Game.Monsters.MGunnerGlobals     as X
import Game.Monsters.MHoverGlobals      as X
import Game.Monsters.MInfantryGlobals   as X
import Game.Monsters.MInsaneGlobals     as X
import Game.Monsters.MMedicGlobals      as X
import Game.Monsters.MMutantGlobals     as X
import Game.Monsters.MParasiteGlobals   as X
import Game.Monsters.MSoldierGlobals    as X
import Game.Monsters.MSuperTankGlobals  as X
import Game.Monsters.MTankGlobals       as X
import Game.PlayerTrailGlobals          as X
import Globals                          as X
import QCommon.CMGlobals                as X
import QCommon.ComGlobals               as X
import QCommon.FSGlobals                as X
import QCommon.NetChannelGlobals        as X
import QCommon.PMoveGlobals             as X
import Render.Fast.FastRenderAPIGlobals as X
import Render.GLFWbGlobals              as X
import Server.SVGlobals                 as X
import Sys.INGlobals                    as X
import Sys.KBDGlobals                   as X
import Sys.NETGlobals                   as X
import Types

import Control.Lens (makeLenses)

makeLenses ''QuakeState

worldRef :: Ref EdictT
worldRef = Ref 0

initialQuakeState :: QuakeState
initialQuakeState = QuakeState
    { _globals              = initialGlobals
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
    , _scrGlobals           = initialSCRGlobals
    , _netGlobals           = initialNETGlobals
    , _playerTrailGlobals   = initialPlayerTrailGlobals
    , _vGlobals             = initialVGlobals
    , _netChannelGlobals    = initialNetChannelGlobals
    , _clTEntGlobals        = initialCLTEntGlobals
    , _mBerserkGlobals      = initialMBerserkGlobals
    , _mBoss2Globals        = initialMBoss2Globals
    , _mBoss31Globals       = initialMBoss31Globals
    , _mBoss32Globals       = initialMBoss32Globals
    , _mBrainGlobals        = initialMBrainGlobals
    , _mChickGlobals        = initialMChickGlobals
    , _mFlipperGlobals      = initialMFlipperGlobals
    , _mFloatGlobals        = initialMFloatGlobals
    , _mFlyerGlobals        = initialMFlyerGlobals
    , _mGladiatorGlobals    = initialMGladiatorGlobals
    , _mGunnerGlobals       = initialMGunnerGlobals
    , _mHoverGlobals        = initialMHoverGlobals
    , _mInfantryGlobals     = initialMInfantryGlobals
    , _mInsaneGlobals       = initialMInsaneGlobals
    , _mMedicGlobals        = initialMMedicGlobals
    , _mMutantGlobals       = initialMMutantGlobals
    , _mParasiteGlobals     = initialMParasiteGlobals
    , _mSoldierGlobals      = initialMSoldierGlobals
    , _mSuperTankGlobals    = initialMSuperTankGlobals
    , _mTankGlobals         = initialMTankGlobals
    }
