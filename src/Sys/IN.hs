{-# LANGUAGE OverloadedStrings #-}
module Sys.IN where

import Control.Lens ((.=), use, (^.), preuse, ix, (+=), (-=))
import Control.Monad (void, when, liftM)
import Data.Bits ((.&.), shiftL)
import Linear (_x, _y, _z)

import Client.FrameT
import Game.PlayerStateT
import Sys.KBD
import Types
import Game.UserCmdT
import Game.PMoveStateT
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.KeyConstants as KeyConstants
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

toggleMouse :: XCommandT
toggleMouse =
  XCommandT "IN.toggleMouse" (do
    mouseAvail <- use $ inGlobals.inMouseAvail

    if mouseAvail
      then do
        inGlobals.inMouseAvail .= False
        deactivateMouse
      else do
        inGlobals.inMouseAvail .= True
        activateMouse
  )

mLookDown :: XCommandT
mLookDown = XCommandT "IN.mLookDown" (inGlobals.inMLooking .= True)

mLookUp :: XCommandT
mLookUp =
  XCommandT "IN.mLookUp" (do
    inGlobals.inMLooking .= False
    (centerView)^.xcCmd
  )

forceCenterViewF :: XCommandT
forceCenterViewF =
  XCommandT "IN.forceCenterViewF" (do
    let access = case Constants.pitch of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen

    globals.gCl.csViewAngles.(access) .= 0
  )

activateMouse :: Quake ()
activateMouse = do
    mouseAvail <- use $ inGlobals.inMouseAvail
    mouseActive <- use $ inGlobals.inMouseActive

    when (mouseAvail && not mouseActive) $ do
      -- don't spazz
      kbdGlobals.kbdMx .= 0
      kbdGlobals.kbdMy .= 0

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
    Just renderer <- use $ globals.gRenderer
    renderer^.rRefExport.reGetKeyboardHandler.kbdInstallGrabs
    inGlobals.inIgnoreFirst .= True

uninstallGrabs :: Quake ()
uninstallGrabs = do
    Just renderer <- use $ globals.gRenderer
    renderer^.rRefExport.reGetKeyboardHandler.kbdUninstallGrabs

centerView :: XCommandT
centerView =
  XCommandT "IN.centerView" (do
    let access = case Constants.pitch of
                   0 -> _x
                   1 -> _y
                   2 -> _z
                   _ -> undefined -- shouldn't happen

    Just angle <- preuse $ globals.gCl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles.(Math3D.v3Access Constants.pitch)
    globals.gCl.csViewAngles.(access) .= (- (Math3D.shortToAngle (fromIntegral angle)))
  )

frame :: Quake ()
frame = do
    cl' <- use $ globals.gCl
    keyDest <- use $ globals.gCls.csKeyDest

    if not (cl'^.csCinematicPaletteActive) && (not (cl'^.csRefreshPrepped) || keyDest == Constants.keyConsole || keyDest == Constants.keyMenu)
      then deactivateMouse
      else activateMouse

commands :: Quake ()
commands = do
    mouseAvail <- use $ inGlobals.inMouseAvail

    when mouseAvail $ do
      Just renderer <- use $ globals.gRenderer
      let kbd = renderer^.rRefExport.reGetKeyboardHandler
      checkMouseButtonState kbd 0 3
      mouseButtonState <- use $ inGlobals.inMouseButtonState
      inGlobals.inMouseOldButtonState .= mouseButtonState

  where checkMouseButtonState :: KBD -> Int -> Int -> Quake ()
        checkMouseButtonState kbd idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              mouseButtonState <- use $ inGlobals.inMouseButtonState
              mouseOldButtonState <- use $ inGlobals.inMouseOldButtonState

              when ((mouseButtonState .&. (1 `shiftL` idx) /= 0) && (mouseOldButtonState .&. (1 `shiftL` idx) == 0)) $
                (kbd^.kbdDoKeyEvent) (KeyConstants.kMouse1 + idx) True

              when ((mouseButtonState .&. (1 `shiftL` idx) == 0) && (mouseOldButtonState .&. (1 `shiftL` idx) /= 0)) $
                (kbd^.kbdDoKeyEvent) (KeyConstants.kMouse1 + idx) False

move :: UserCmdReference -> Quake ()
move (UserCmdReference cmdIdx) = do
    filterValue <- liftM (^.cvValue) mFilterCVar

    when (filterValue /= 0) $ do
      mx <- use $ kbdGlobals.kbdMx
      my <- use $ kbdGlobals.kbdMy
      oldMouseX <- use $ inGlobals.inOldMouseX
      oldMouseY <- use $ inGlobals.inOldMouseY
      kbdGlobals.kbdMx .= (mx + oldMouseX) `div` 2
      kbdGlobals.kbdMy .= (my + oldMouseY) `div` 2

    use (kbdGlobals.kbdMx) >>= \v -> inGlobals.inOldMouseX .= v
    use (kbdGlobals.kbdMy) >>= \v -> inGlobals.inOldMouseY .= v

    sensitivityValue <- liftM (^.cvValue) sensitivityCVar
    use (kbdGlobals.kbdMx) >>= \v ->
      kbdGlobals.kbdMx .= truncate (fromIntegral v * sensitivityValue)
    use (kbdGlobals.kbdMy) >>= \v ->
      kbdGlobals.kbdMy .= truncate (fromIntegral v * sensitivityValue) 

    -- add mouse X/Y movement to cmd
    inStrafe <- use $ clientGlobals.cgInStrafe
    lookStrafeValue <- liftM (^.cvValue) lookStrafeCVar
    looking <- use $ inGlobals.inMLooking

    mx <- use $ kbdGlobals.kbdMx
    my <- use $ kbdGlobals.kbdMy

    if (inStrafe^.kbState) .&. 1 /= 0 || (lookStrafeValue /= 0 && looking)
      then do
        sideValue <- liftM (^.cvValue) mSideCVar
        globals.gCl.csCmds.ix cmdIdx.ucSideMove += truncate (sideValue * fromIntegral mx)
      else do
        yawValue <- liftM (^.cvValue) mYawCVar
        let access = case Constants.yaw of
                       0 -> _x
                       1 -> _y
                       2 -> _z
                       _ -> undefined -- shouldn't happen
        globals.gCl.csViewAngles.access -= yawValue * fromIntegral mx

    freeLookValue <- liftM (^.cvValue) freeLookCVar

    if (looking || freeLookValue /= 0) && ((inStrafe^.kbState) .&. 1 == 0)
      then do
        pitchValue <- liftM (^.cvValue) mPitchCVar
        let access = case Constants.pitch of
                       0 -> _x
                       1 -> _y
                       2 -> _z
                       _ -> undefined -- shouldn't happen
        globals.gCl.csViewAngles.access += pitchValue * fromIntegral my
      else do
        forwardValue <- liftM (^.cvValue) mForwardCVar
        globals.gCl.csCmds.ix cmdIdx.ucForwardMove -= truncate (forwardValue * fromIntegral my)

    kbdGlobals.kbdMx .= 0
    kbdGlobals.kbdMy .= 0
