{-# LANGUAGE FlexibleContexts #-}
module Sys.IN
    ( centerView
    , commands
    , frame
    , initialize
    , move
    , realINInit
    , shutdown
    ) where

import           Control.Lens          (use, (^.), (.=), (%=), (-=), (+=), (&), (+~), (-~))
import           Control.Monad         (when)
import           Data.Bits             (shiftL, (.&.))
import qualified Data.ByteString       as B
import           Linear                (_x, _y)

import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.FrameT
import           Client.KButtonT
import qualified Client.KeyConstants   as KeyConstants
import           Client.RefExportT
import qualified Constants
import qualified Game.Cmd              as Cmd
import           Game.CVarT
import           Game.PlayerStateT
import           Game.PMoveStateT
import           Game.UserCmdT
import qualified QCommon.CVar          as CVar
import           QCommon.CVarVariables
import           QCommon.XCommandT     (runXCommandT)
import           QuakeRef
import           QuakeState
import           Render.Renderer
import           Sys.KBD
import           Types
import qualified Util.Math3D           as Math3D

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
    [ ("in_mouse", "1", Constants.cvarArchive)
    , ("in_joystick", "0", Constants.cvarArchive)
    ]

otherInitialCVars :: [(B.ByteString, B.ByteString, Int)]
otherInitialCVars =
    [ ("m_filter", "0", 0)
    , ("in_mouse", "1", Constants.cvarArchive)
    , ("freelook", "1", 0)
    , ("lookstrafe", "0", 0)
    , ("sensitivity", "3", 0)
    , ("m_pitch", "0.022", 0)
    , ("m_yaw", "0.022", 0)
    , ("m_forward", "1", 0)
    , ("m_side", "0.8", 0)
    ]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
    [ ("+mlook", Just mLookDown)
    , ("-mlook", Just mLookUp)
    , ("force_centerview", Just forceCenterViewF)
    , ("togglemouse", Just toggleMouse)
    ]

centerView :: XCommandT
centerView = XCommandT "IN.centerView" $ do
    deltaAngles <- use (globals.gCl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles)
    globals.gCl.csViewAngles._x .= (negate (Math3D.shortToAngle (fromIntegral (deltaAngles^._x))))

commands :: Quake ()
commands = do
    mouseAvail <- use (inGlobals.inMouseAvail)
    when mouseAvail $ do
        renderer <- use (globals.gRenderer)
        checkMouseButtonState (renderer^.rRefExport.reGetKeyboardHandler) 0 3
        mouseButtonState <- use (inGlobals.inMouseButtonState)
        inGlobals.inMouseOldButtonState .= mouseButtonState

checkMouseButtonState :: KBD -> Int -> Int -> Quake ()
checkMouseButtonState kbd idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        mouseButtonState <- use (inGlobals.inMouseButtonState)
        mouseOldButtonState <- use (inGlobals.inMouseOldButtonState)
        when ((mouseButtonState .&. (1 `shiftL` idx) /= 0) && (mouseOldButtonState .&. (1 `shiftL` idx) == 0)) $
            (kbd^.kbdDoKeyEvent) (KeyConstants.kMouse1 + idx) True
        when ((mouseButtonState .&. (1 `shiftL` idx) == 0) && (mouseOldButtonState .&. (1 `shiftL` idx) /= 0)) $
            (kbd^.kbdDoKeyEvent) (KeyConstants.kMouse1 + idx) False
        checkMouseButtonState kbd (idx + 1) maxIdx

frame :: Quake ()
frame = do
    cl <- use (globals.gCl)
    keyDest <- use (globals.gCls.csKeyDest)
    runFrame cl keyDest
  where
    runFrame cl keyDest
        | not (cl^.csCinematicPaletteActive) && (not (cl^.csRefreshPrepped) || keyDest == Constants.keyConsole || keyDest == Constants.keyMenu) =
            deactivateMouse
        | otherwise = activateMouse

activateMouse :: Quake ()
activateMouse = do
    mouseAvail <- use (inGlobals.inMouseAvail)
    mouseActive <- use (inGlobals.inMouseActive)
    when (mouseAvail && not mouseActive) $ do
        kbdGlobals.kbdMx .= 0
        kbdGlobals.kbdMy .= 0
        installGrabs
        inGlobals.inMouseActive .= True

deactivateMouse :: Quake ()
deactivateMouse = do
    mouseActive <- use (inGlobals.inMouseActive)
    when mouseActive $ do
        uninstallGrabs
        inGlobals.inMouseActive .= False

installGrabs :: Quake ()
installGrabs = do
    renderer <- use (globals.gRenderer)
    renderer^.rRefExport.reGetKeyboardHandler.kbdInstallGrabs
    inGlobals.inIgnoreFirst .= True

uninstallGrabs :: Quake ()
uninstallGrabs = do
    renderer <- use (globals.gRenderer)
    renderer^.rRefExport.reGetKeyboardHandler.kbdUninstallGrabs

initialize :: Quake ()
initialize = CVar.initializeCVars initialCVars

move :: Ref UserCmdT -> Quake ()
move cmdRef = do
    checkFilter =<< mFilterCVar
    updateOldMouse
    applySensitivity
    applyMouseXYMovement cmdRef
    kbdGlobals.kbdMx .= 0
    kbdGlobals.kbdMy .= 0
  where
    checkFilter mFilter
        | (mFilter^.cvValue) /= 0 = do
            oldMouseX <- use (inGlobals.inOldMouseX)
            oldMouseY <- use (inGlobals.inOldMouseY)
            kbdGlobals.kbdMx %= (\v -> (v + oldMouseX) `div` 2)
            kbdGlobals.kbdMy %= (\v -> (v + oldMouseY) `div` 2)
        | otherwise = return ()
    updateOldMouse = do
        mx <- use (kbdGlobals.kbdMx)
        my <- use (kbdGlobals.kbdMy)
        inGlobals.inOldMouseX .= mx
        inGlobals.inOldMouseY .= my
    applySensitivity = do
        sensitivity <- sensitivityCVar
        kbdGlobals.kbdMx %= (\v -> truncate (fromIntegral v * (sensitivity^.cvValue)))
        kbdGlobals.kbdMy %= (\v -> truncate (fromIntegral v * (sensitivity^.cvValue)))

applyMouseXYMovement :: Ref UserCmdT -> Quake ()
applyMouseXYMovement cmdRef = do
    inStrafe <- use (clientGlobals.cgInStrafe)
    looking <- use (inGlobals.inMLooking)
    lookStrafe <- lookStrafeCVar
    freeLook <- freeLookCVar
    mx <- use (kbdGlobals.kbdMx)
    my <- use (kbdGlobals.kbdMy)
    checkInStrafe cmdRef inStrafe looking lookStrafe mx
    checkFreeLook cmdRef inStrafe looking freeLook my

checkInStrafe :: Ref UserCmdT -> KButtonT -> Bool -> CVarT -> Int -> Quake ()
checkInStrafe cmdRef inStrafe looking lookStrafe mx
    | (inStrafe^.kbState) .&. 1 /= 0 || ((lookStrafe^.cvValue) /= 0 && looking) = do
        side <- mSideCVar
        modifyRef cmdRef (\v -> v & ucSideMove +~ truncate ((side^.cvValue) * fromIntegral mx))
    | otherwise = do
        yaw <- mYawCVar
        globals.gCl.csViewAngles._y -= (yaw^.cvValue) * fromIntegral mx

checkFreeLook :: Ref UserCmdT -> KButtonT -> Bool -> CVarT -> Int -> Quake ()
checkFreeLook cmdRef inStrafe looking freeLook my
    | (inStrafe^.kbState) .&. 1 == 0 && ((freeLook^.cvValue) /= 0 && looking) = do
        pitch <- mPitchCVar
        globals.gCl.csViewAngles._x += (pitch^.cvValue) * fromIntegral my
    | otherwise = do
        forward <- mForwardCVar
        modifyRef cmdRef (\v -> v & ucForwardMove -~ truncate ((forward^.cvValue) * fromIntegral my))

realINInit :: Quake ()
realINInit = do
    CVar.initializeCVars otherInitialCVars
    Cmd.addInitialCommands initialCommands
    inGlobals.inMouseAvail .= True

mLookDown :: XCommandT
mLookDown = XCommandT "IN.mLookDown" (inGlobals.inMLooking .= True)

mLookUp :: XCommandT
mLookUp = XCommandT "IN.mLookUp" $ do
    inGlobals.inMLooking .= False
    runXCommandT centerView

forceCenterViewF :: XCommandT
forceCenterViewF = XCommandT "IN.forceCenterViewF" $
    globals.gCl.csViewAngles._x .= 0

toggleMouse :: XCommandT
toggleMouse = XCommandT "IN.toggleMouse" $ do
    mouseAvail <- use (inGlobals.inMouseAvail)
    doToggleMouse mouseAvail
  where
    doToggleMouse True = do
        inGlobals.inMouseAvail .= False
        deactivateMouse
    doToggleMouse False = do
        inGlobals.inMouseAvail .= True
        activateMouse

shutdown :: Quake ()
shutdown = inGlobals.inMouseAvail .= False
