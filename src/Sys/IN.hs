{-# LANGUAGE OverloadedStrings #-}
module Sys.IN where

import Control.Lens ((.=), use, (^.), preuse)
import Control.Monad (void, when)
import Linear (_x, _y, _z)

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import qualified Util.Math3D as Math3D

init :: Quake ()
init = do
    void $ CVar.get "in_mouse" "1" Constants.cvarArchive
    void $ CVar.get "in_joystick" "0" Constants.cvarArchive

shutdown :: Quake ()
shutdown = do
    inGlobals.inMouseAvail .= False

realINInit :: Quake ()
realINInit = do
    -- mouse variables
    void $ CVar.get "m_filter" "0" 0
    void $ CVar.get "in_mouse" "1" Constants.cvarArchive
    void $ CVar.get "freelook" "1" 0
    void $ CVar.get "lookstrafe" "0" 0
    void $ CVar.get "sensitivity" "3" 0
    void $ CVar.get "m_pitch" "0.022" 0
    void $ CVar.get "m_yaw" "0.022" 0
    void $ CVar.get "m_forward" "1" 0
    void $ CVar.get "m_side" "0.8" 0

    Cmd.addCommand "+mlook" (Just mLookDown)
    Cmd.addCommand "-mlook" (Just mLookUp)
    Cmd.addCommand "force_centerview" (Just forceCenterViewF)
    Cmd.addCommand "togglemouse" (Just toggleMouse)

    inGlobals.inMouseAvail .= True

toggleMouse :: Quake ()
toggleMouse = do
    mouseAvail <- use $ inGlobals.inMouseAvail

    if mouseAvail
      then do
        inGlobals.inMouseAvail .= False
        deactivateMouse
      else do
        inGlobals.inMouseAvail .= True
        activateMouse

mLookDown :: Quake ()
mLookDown = inGlobals.inMLooking .= True

mLookUp :: Quake ()
mLookUp = do
    inGlobals.inMLooking .= False
    centerView

forceCenterViewF :: XCommandT
forceCenterViewF = do
    let access = case Constants.pitch of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen

    globals.cl.csViewAngles.(access) .= 0

activateMouse :: Quake ()
activateMouse = do
    mouseAvail <- use $ inGlobals.inMouseAvail
    mouseActive <- use $ inGlobals.inMouseActive

    when (mouseAvail && not mouseActive) $ do
      -- don't spazz
      glfwbKBDGlobals.glfwbKBDmx .= 0
      glfwbKBDGlobals.glfwbKBDmy .= 0

      installGrabs

      inGlobals.inMouseActive .= True

deactivateMouse :: Quake ()
deactivateMouse = do
    mouseActive <- use $ inGlobals.inMouseActive

    when mouseActive $ do
      uninstallGrabs
      inGlobals.inMouseActive .= False

installGrabs :: Quake ()
installGrabs = do
    Just renderer <- use $ globals.re
    renderer^.rRefExport.reGetKeyboardHandler.kbdInstallGrabs
    inGlobals.inIgnoreFirst .= True

uninstallGrabs :: Quake ()
uninstallGrabs = do
    Just renderer <- use $ globals.re
    renderer^.rRefExport.reGetKeyboardHandler.kbdUninstallGrabs

centerView :: Quake ()
centerView = do
    let access = case Constants.pitch of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen

    Just angle <- preuse $ globals.cl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles.(Math3D.v3Access Constants.pitch)
    globals.cl.csViewAngles.(access) .= (- (Math3D.shortToAngle angle))

frame :: Quake ()
frame = do
    cl' <- use $ globals.cl
    keyDest <- use $ globals.cls.csKeyDest

    if not (cl'^.csCinematicPaletteActive) && (not (cl'^.csRefreshPrepped) || keyDest == Constants.keyConsole || keyDest == Constants.keyMenu)
      then deactivateMouse
      else activateMouse

commands :: Quake ()
commands = io (putStrLn "IN.commands") >> undefined -- TODO
