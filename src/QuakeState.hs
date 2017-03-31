{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
                  , mBoss2Globals
                  , mBoss31Globals
                  , mBoss32Globals
                  , mBrainGlobals
                  , mChickGlobals
                  , mFlipperGlobals
                  , mFloatGlobals
                  , mFlyerGlobals
                  , mGladiatorGlobals
                  , mGunnerGlobals
                  , mHoverGlobals
                  , mInfantryGlobals
                  , mInsaneGlobals
                  , mMedicGlobals
                  , mMutantGlobals
                  , mParasiteGlobals
                  , mSoldierGlobals
                  , mSuperTankGlobals
                  , mTankGlobals
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
                  , worldRef
                  , ClientReference(..)
                  , GClientReference(..)
                  , CModelReference(..)
                  , LinkReference(..)
                  , GItemReference(..)
                  , UserCmdReference(..)
                  , GLPolyReference(..)
                  , MenuFrameworkSReference
                  , MenuActionSReference
                  , MenuListSReference
                  , MenuFieldSReference
                  , MenuSliderSReference
                  , MenuSeparatorSReference
                  , MenuItemReference(..)
                  , CPlaneReference
                  , newCPlaneReference
                  , readCPlaneT
                  , modifyCPlaneT
                  , writeCPlaneT
                  , CBrushReference
                  , newCBrushReference
                  , readCBrushT
                  , modifyCBrushT
                  , writeCBrushT
                  , readMenuFrameworkSReference
                  , modifyMenuFrameworkSReference
                  , writeMenuFrameworkSReference
                  , readMenuListSReference
                  , modifyMenuListSReference
                  , writeMenuListSReference
                  , readMenuActionSReference
                  , modifyMenuActionSReference
                  , writeMenuActionSReference
                  , readMenuSliderSReference
                  , modifyMenuSliderSReference
                  , writeMenuSliderSReference
                  , readMenuSeparatorSReference
                  , modifyMenuSeparatorSReference
                  , writeMenuSeparatorSReference
                  , readMenuFieldSReference
                  , modifyMenuFieldSReference
                  , writeMenuFieldSReference
                  , MTexInfoReference(..)
                  , MNodeReference(..)
                  , SfxReference(..)
                  , MNodeChild(..)
                  , ModelExtra(..)
                  , EntThinkAdapter(..)
                  , EntBlockedAdapter(..)
                  , EntDodgeAdapter(..)
                  , EntTouchAdapter(..)
                  , EntUseAdapter(..)
                  , EntPainAdapter(..)
                  , EntInteractAdapter(..)
                  , EntDieAdapter(..)
                  , ItemUseAdapter(..)
                  , ItemDropAdapter(..)
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
                  , module Game.Monsters.MFlipperGlobals
                  , module Game.Monsters.MFloatGlobals
                  , module Game.Monsters.MFlyerGlobals
                  , module Game.Monsters.MGladiatorGlobals
                  , module Game.Monsters.MGunnerGlobals
                  , module Game.Monsters.MHoverGlobals
                  , module Game.Monsters.MInfantryGlobals
                  , module Game.Monsters.MInsaneGlobals
                  , module Game.Monsters.MMedicGlobals
                  , module Game.Monsters.MMutantGlobals
                  , module Game.Monsters.MParasiteGlobals
                  , module Game.Monsters.MSoldierGlobals
                  , module Game.Monsters.MSuperTankGlobals
                  , module Game.Monsters.MTankGlobals
                  , module Game.PlayerTrailGlobals
                  , module Client.VIDGlobals
                  , module Render.Basic.BasicRenderAPIGlobals
                  , module Render.Fast.FastRenderAPIGlobals
                  , module Render.GLFWbGlobals
                  , module QCommon.NetChannelGlobals
                  ) where

import Control.Lens (use, makeLenses, ix, preuse, (%=), (.=))
import Control.Monad.State.Strict (liftIO)
import qualified Data.Vector.Mutable as MV

import Types
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
import Game.Monsters.MFlipperGlobals
import Game.Monsters.MFloatGlobals
import Game.Monsters.MFlyerGlobals
import Game.Monsters.MGladiatorGlobals
import Game.Monsters.MGunnerGlobals
import Game.Monsters.MHoverGlobals
import Game.Monsters.MInfantryGlobals
import Game.Monsters.MInsaneGlobals
import Game.Monsters.MMedicGlobals
import Game.Monsters.MMutantGlobals
import Game.Monsters.MParasiteGlobals
import Game.Monsters.MSoldierGlobals
import Game.Monsters.MSuperTankGlobals
import Game.Monsters.MTankGlobals
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
             , _mBoss2Globals         = initialMBoss2Globals
             , _mBoss31Globals        = initialMBoss31Globals
             , _mBoss32Globals        = initialMBoss32Globals
             , _mBrainGlobals         = initialMBrainGlobals
             , _mChickGlobals         = initialMChickGlobals
             , _mFlipperGlobals       = initialMFlipperGlobals
             , _mFloatGlobals         = initialMFloatGlobals
             , _mFlyerGlobals         = initialMFlyerGlobals
             , _mGladiatorGlobals     = initialMGladiatorGlobals
             , _mGunnerGlobals        = initialMGunnerGlobals
             , _mHoverGlobals         = initialMHoverGlobals
             , _mInfantryGlobals      = initialMInfantryGlobals
             , _mInsaneGlobals        = initialMInsaneGlobals
             , _mMedicGlobals         = initialMMedicGlobals
             , _mMutantGlobals        = initialMMutantGlobals
             , _mParasiteGlobals      = initialMParasiteGlobals
             , _mSoldierGlobals       = initialMSoldierGlobals
             , _mSuperTankGlobals     = initialMSuperTankGlobals
             , _mTankGlobals          = initialMTankGlobals
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

worldRef :: Ref EdictT
worldRef = Ref 0

readCPlaneT :: CPlaneReference -> Quake CPlaneT
readCPlaneT (CPlaneReference planeIdx) = do
    mapPlanes <- use $ cmGlobals.cmMapPlanes
    liftIO $ MV.read mapPlanes planeIdx

modifyCPlaneT :: CPlaneReference -> (CPlaneT -> CPlaneT) -> Quake ()
modifyCPlaneT (CPlaneReference planeIdx) f = do
    mapPlanes <- use $ cmGlobals.cmMapPlanes
    liftIO $ MV.modify mapPlanes f planeIdx

writeCPlaneT :: CPlaneReference -> CPlaneT -> Quake ()
writeCPlaneT (CPlaneReference planeIdx) plane = do
    mapPlanes <- use $ cmGlobals.cmMapPlanes
    liftIO $ MV.write mapPlanes planeIdx plane

newCPlaneReference :: Int -> CPlaneReference
newCPlaneReference = CPlaneReference

readCBrushT :: CBrushReference -> Quake CBrushT
readCBrushT (CBrushReference brushIdx) = do
    mapBrushes <- use $ cmGlobals.cmMapBrushes
    liftIO $ MV.read mapBrushes brushIdx

modifyCBrushT :: CBrushReference -> (CBrushT -> CBrushT) -> Quake ()
modifyCBrushT (CBrushReference brushIdx) f = do
    mapBrushes <- use $ cmGlobals.cmMapBrushes
    liftIO $ MV.modify mapBrushes f brushIdx

writeCBrushT :: CBrushReference -> CBrushT -> Quake ()
writeCBrushT (CBrushReference brushIdx) brush = do
    mapBrushes <- use $ cmGlobals.cmMapBrushes
    liftIO $ MV.write mapBrushes brushIdx brush

newCBrushReference :: Int -> CBrushReference
newCBrushReference = CBrushReference

readMenuFrameworkSReference :: MenuFrameworkSReference -> Quake MenuFrameworkS
readMenuFrameworkSReference (MenuFrameworkSReference menuFrameworkIdx) = do
    Just menu <- preuse $ menuGlobals.mgMenuFrameworks.ix menuFrameworkIdx
    return menu

modifyMenuFrameworkSReference :: MenuFrameworkSReference -> (MenuFrameworkS -> MenuFrameworkS) -> Quake ()
modifyMenuFrameworkSReference (MenuFrameworkSReference menuFrameworkIdx) f =
    menuGlobals.mgMenuFrameworks.ix menuFrameworkIdx %= f

writeMenuFrameworkSReference :: MenuFrameworkSReference -> MenuFrameworkS -> Quake ()
writeMenuFrameworkSReference (MenuFrameworkSReference menuFrameworkIdx) menu =
    menuGlobals.mgMenuFrameworks.ix menuFrameworkIdx .= menu

readMenuListSReference :: MenuListSReference -> Quake MenuListS
readMenuListSReference (MenuListSReference idx) = do
    Just menuItem <- preuse $ menuGlobals.mgMenuListSItems.ix idx
    return menuItem

modifyMenuListSReference :: MenuListSReference -> (MenuListS -> MenuListS) -> Quake ()
modifyMenuListSReference (MenuListSReference idx) f =
    menuGlobals.mgMenuListSItems.ix idx %= f

writeMenuListSReference :: MenuListSReference -> MenuListS -> Quake ()
writeMenuListSReference (MenuListSReference idx) menuItem =
    menuGlobals.mgMenuListSItems.ix idx .= menuItem

readMenuActionSReference :: MenuActionSReference -> Quake MenuActionS
readMenuActionSReference (MenuActionSReference idx) = do
    Just menuItem <- preuse $ menuGlobals.mgMenuActionSItems.ix idx
    return menuItem

modifyMenuActionSReference :: MenuActionSReference -> (MenuActionS -> MenuActionS) -> Quake ()
modifyMenuActionSReference (MenuActionSReference idx) f =
    menuGlobals.mgMenuActionSItems.ix idx %= f

writeMenuActionSReference :: MenuActionSReference -> MenuActionS -> Quake ()
writeMenuActionSReference (MenuActionSReference idx) menuItem =
    menuGlobals.mgMenuActionSItems.ix idx .= menuItem

readMenuSliderSReference :: MenuSliderSReference -> Quake MenuSliderS
readMenuSliderSReference (MenuSliderSReference idx) = do
    Just menuItem <- preuse $ menuGlobals.mgMenuSliderSItems.ix idx
    return menuItem

modifyMenuSliderSReference :: MenuSliderSReference -> (MenuSliderS -> MenuSliderS) -> Quake ()
modifyMenuSliderSReference (MenuSliderSReference idx) f =
    menuGlobals.mgMenuSliderSItems.ix idx %= f

writeMenuSliderSReference :: MenuSliderSReference -> MenuSliderS -> Quake ()
writeMenuSliderSReference (MenuSliderSReference idx) menuItem =
    menuGlobals.mgMenuSliderSItems.ix idx .= menuItem

readMenuSeparatorSReference :: MenuSeparatorSReference -> Quake MenuSeparatorS
readMenuSeparatorSReference (MenuSeparatorSReference idx) = do
    Just menuItem <- preuse $ menuGlobals.mgMenuSeparatorSItems.ix idx
    return menuItem

modifyMenuSeparatorSReference :: MenuSeparatorSReference -> (MenuSeparatorS -> MenuSeparatorS) -> Quake ()
modifyMenuSeparatorSReference (MenuSeparatorSReference idx) f =
    menuGlobals.mgMenuSeparatorSItems.ix idx %= f

writeMenuSeparatorSReference :: MenuSeparatorSReference -> MenuSeparatorS -> Quake ()
writeMenuSeparatorSReference (MenuSeparatorSReference idx) menuItem =
    menuGlobals.mgMenuSeparatorSItems.ix idx .= menuItem

readMenuFieldSReference :: MenuFieldSReference -> Quake MenuFieldS
readMenuFieldSReference (MenuFieldSReference idx) = do
    Just menuItem <- preuse $ menuGlobals.mgMenuFieldSItems.ix idx
    return menuItem

modifyMenuFieldSReference :: MenuFieldSReference -> (MenuFieldS -> MenuFieldS) -> Quake ()
modifyMenuFieldSReference (MenuFieldSReference idx) f =
    menuGlobals.mgMenuFieldSItems.ix idx %= f

writeMenuFieldSReference :: MenuFieldSReference -> MenuFieldS -> Quake ()
writeMenuFieldSReference (MenuFieldSReference idx) menuItem =
    menuGlobals.mgMenuFieldSItems.ix idx .= menuItem
