module Render.GLFWbRenderer
  ( glfwbRefExport
  , glfwbRenderer
  ) where

import qualified Client.Key as Key
import qualified Client.KeyConstants as KeyConstants
import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import qualified QCommon.CBuf as CBuf
import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import           Render.Basic.BasicRenderAPI (basicRenderAPI)
import           Render.OpenGL.GLDriver
import           Render.OpenGL.GLFWbGLDriver (glfwbGLDriver)
import           Render.RenderAPI
import qualified Sys.Timer as Timer
import           Types

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, isEmptyTChan, writeTChan)
import           Control.Lens (use, (^.), (.=))
import           Control.Monad (when, unless)
import           Data.Char (ord)
import           Data.Maybe (fromMaybe)
import qualified Graphics.UI.GLFW as GLFW

glfwbRefExport :: RenderAPI -> RefExportT
glfwbRefExport = glfwbRefExportT glfwbKBD

glfwbRenderer :: Renderer
glfwbRenderer = 
  Renderer { _rName      = "GLFWb"
           , _rRefExport = glfwbRefExportT glfwbKBD basicRenderAPI
           }

glfwbRefExportT :: KBD -> RenderAPI -> RefExportT
glfwbRefExportT kbd renderAPI =
  RefExportT { _reInit                = glfwbInit renderAPI
             , _reShutDown            = (renderAPI^.rShutdown) glfwbGLDriver
             , _reBeginRegistration   = (renderAPI^.rBeginRegistration) glfwbGLDriver
             , _reRegisterModel       = (renderAPI^.rRegisterModel) glfwbGLDriver
             , _reRegisterSkin        = (renderAPI^.rRegisterSkin) glfwbGLDriver
             , _reRegisterPic         = (renderAPI^.rDrawFindPic) glfwbGLDriver
             , _reSetSky              = (renderAPI^.rSetSky) glfwbGLDriver
             , _reEndRegistration     = (renderAPI^.rEndRegistration) glfwbGLDriver
             , _reRenderFrame         = (renderAPI^.rRenderFrame) glfwbGLDriver
             , _reDrawGetPicSize      = (renderAPI^.rDrawGetPicSize) glfwbGLDriver
             , _reDrawPic             = (renderAPI^.rDrawPic) glfwbGLDriver
             , _reDrawStretchPic      = (renderAPI^.rDrawStretchPic) glfwbGLDriver
             , _reDrawChar            = (renderAPI^.rDrawChar) glfwbGLDriver
             , _reDrawTileClear       = (renderAPI^.rDrawTileClear) glfwbGLDriver
             , _reDrawFill            = (renderAPI^.rDrawFill) glfwbGLDriver
             , _reDrawFadeScreen      = (renderAPI^.rDrawFadeScreen) glfwbGLDriver
             , _reDrawStretchRaw      = (renderAPI^.rDrawStretchRaw) glfwbGLDriver
             , _reCinematicSetPalette = (renderAPI^.rSetPalette) glfwbGLDriver
             , _reBeginFrame          = (renderAPI^.rBeginFrame) glfwbGLDriver
             , _reEndFrame            = glfwbGLDriver^.gldEndFrame
             , _reAppActivate         = glfwbAppActivate
             , _reUpdateScreen        = runXCommandT
             , _reApiVersion          = Constants.apiVersion
             , _reGetModeList         = glfwbGLDriver^.gldGetModeList
             , _reGetKeyboardHandler  = kbd
             }

glfwbKBD :: KBD
glfwbKBD =
  KBD { _kbdInit           = glfwbKBDInit
      , _kbdUpdate         = glfwbKBDUpdate
      , _kbdClose          = return () -- TODO: make sure this is correct
      , _kbdDoKeyEvent     = \_ _ -> error "glfwbKBD.kbdDoKeyEvent" -- TODO
      , _kbdInstallGrabs   = glfwbKBDInstallGrabs
      , _kbdUninstallGrabs = glfwbKBDUninstallGrabs
      }

glfwbInit :: RenderAPI -> Int -> Int -> Quake Bool
glfwbInit renderAPI vidXPos vidYPos =
  request (io GLFW.init) >>= proceedInit
  where proceedInit True =
          (renderAPI^.rInit) glfwbGLDriver vidXPos vidYPos >>= postInit
        proceedInit False =
          do VID.printf Constants.printAll "Failed to initialize GLFW-b\n"
             return False
        postInit False = return False
        postInit True = (renderAPI^.rInit2) glfwbGLDriver

glfwbAppActivate :: Bool -> Quake ()
glfwbAppActivate = error "GLFWbRenderer.glfwbAppActivate" -- TODO

glfwbKBDInit :: Quake ()
glfwbKBDInit =
  do window <- use (glfwbGlobals.glfwbWindow)
     maybe windowError kbdInit window
  where windowError = error "GLFWbRenderer.glfwbKBDInit window is Nothing"
        kbdInit window =
          do (w, h) <- request (io (GLFW.getWindowSize window))
             kbdGlobals.kbdWinW2 .= w `div` 2
             kbdGlobals.kbdWinH2 .= h `div` 2
             initKBDChanAndCallbacks window

initKBDChanAndCallbacks :: GLFW.Window -> Quake ()
initKBDChanAndCallbacks window =
  do kbdChan <- request (io (newTChanIO :: IO (TChan GLFWKBDEvent)))
     glfwbGlobals.glfwbKBDChan .= Just kbdChan
     request (io (initKBDCallbacks window kbdChan))

initKBDCallbacks :: GLFW.Window -> TChan GLFWKBDEvent -> IO ()
initKBDCallbacks window kbdChan =
  do GLFW.setKeyCallback window (Just (keyCallback kbdChan))
     GLFW.setMouseButtonCallback window (Just (mouseButtonCallback kbdChan))
     GLFW.setCursorPosCallback window (Just (cursorPosCallback kbdChan))
     GLFW.setScrollCallback window (Just (scrollCallback kbdChan))

glfwbKBDUpdate :: Quake ()
glfwbKBDUpdate =
  do request (io (GLFW.pollEvents))
     window <- use (glfwbGlobals.glfwbWindow)
     maybe windowError checkWindowClose window
     kbdChan <- use (glfwbGlobals.glfwbKBDChan)
     maybe kbdChanError handleEvents kbdChan
  where windowError = error "GLFWbRenderer.glfwbKBDUpdate window is Nothing"
        checkWindowClose window =
          do shouldClose <- request (io (GLFW.windowShouldClose window))
             when shouldClose $
               CBuf.executeText Constants.execAppend "quit"
        kbdChanError = error "GLFWbRenderer.glfwbKBDUpdate kbdChan is Nothing"

handleEvents :: TChan GLFWKBDEvent -> Quake ()
handleEvents kbdChan =
  do emptyChan <- request (io (atomically (isEmptyTChan kbdChan)))
     unless emptyChan $
       do msg <- request (io (atomically (readTChan kbdChan)))
          handleEvent msg
          handleEvents kbdChan

handleEvent :: GLFWKBDEvent -> Quake ()
handleEvent (KeyPress key) = doKeyEvent (xLateKey key) True
handleEvent (KeyRelease key) = doKeyEvent (xLateKey key) False
handleEvent (CursorPosition x y) = cursorPositionEvent (truncate x) (truncate y)
handleEvent (MouseButtonPress button) = doKeyEvent (mouseEventToKey button) True
handleEvent (MouseButtonRelease button) = doKeyEvent (mouseEventToKey button) False
handleEvent (MouseWheelScroll scroll) = error "GLFWbRenderer.handleEvent MouseWheelScrool" -- TODO
handleEvent ConfigureNotify = error "GLFWbRenderer.handleEvent ConfigureNotify" -- TODO

cursorPositionEvent :: Int -> Int -> Quake ()
cursorPositionEvent x' y' =
  do x <- use (kbdGlobals.kbdX)
     y <- use (kbdGlobals.kbdY)
     kbdGlobals.kbdMx .= (x' - x) * 2
     kbdGlobals.kbdMy .= (y' - y) * 2
     kbdGlobals.kbdX .= x'
     kbdGlobals.kbdY .= y'

glfwbKBDInstallGrabs :: Quake ()
glfwbKBDInstallGrabs = error "GLFWbRenderer.glfwbKBDInstallGrabs" -- TODO

glfwbKBDUninstallGrabs :: Quake ()
glfwbKBDUninstallGrabs = do
    window <- use (glfwbGlobals.glfwbWindow)
    maybe windowError uninstallGrabs window
  where windowError = error "GLFWbRenderer.glfwbKBDUninstallGrabs window is Nothing"
        uninstallGrabs window =
          request (io (GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal))

keyCallback :: TChan GLFWKBDEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback kbdChan _ key _ action _
  | action == GLFW.KeyState'Pressed = atomically (writeTChan kbdChan (KeyPress key))
  | action == GLFW.KeyState'Released = atomically (writeTChan kbdChan (KeyRelease key))
  | otherwise = return ()

mouseButtonCallback :: TChan GLFWKBDEvent -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback kbdChan _ button action _
  | action == GLFW.MouseButtonState'Pressed = atomically (writeTChan kbdChan (MouseButtonPress button))
  | action == GLFW.MouseButtonState'Released = atomically (writeTChan kbdChan (MouseButtonRelease button))
  | otherwise = return ()

cursorPosCallback :: TChan GLFWKBDEvent -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback kbdChan _ xPos yPos =
    atomically (writeTChan kbdChan (CursorPosition xPos yPos))

scrollCallback :: TChan GLFWKBDEvent -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback kbdChan _ _ yOffset =
    -- A simple mouse wheel, being vertical, provides offsets along the Y-axis.
    atomically (writeTChan kbdChan (MouseWheelScroll yOffset))

doKeyEvent :: Int -> Bool -> Quake ()
doKeyEvent key down =
  do msec <- Timer.milliseconds
     Key.event key down msec

-- BUTTON1(left) BUTTON2(center) BUTTON3(right)
-- K_MOUSE1      K_MOUSE3        K_MOUSE2
mouseEventToKey :: GLFW.MouseButton -> Int
mouseEventToKey button
  | button == GLFW.MouseButton'3 = KeyConstants.kMouse3
  | button == GLFW.MouseButton'2 = KeyConstants.kMouse2
  | otherwise = KeyConstants.kMouse1

-- TODO: what about keypad left/right/up/down?
xLateKey :: GLFW.Key -> Int
xLateKey key = fromMaybe 0 (lookup key keys)

keys :: [(GLFW.Key, Int)]
keys =
  [ (GLFW.Key'PageUp, KeyConstants.kPgUp), (GLFW.Key'PageDown, KeyConstants.kPgDn)
  , (GLFW.Key'Home, KeyConstants.kHome), (GLFW.Key'End, KeyConstants.kEnd)
  , (GLFW.Key'Left, KeyConstants.kLeftArrow)
  , (GLFW.Key'Right, KeyConstants.kRightArrow)
  , (GLFW.Key'Down, KeyConstants.kDownArrow)
  , (GLFW.Key'Up, KeyConstants.kUpArrow), (GLFW.Key'Space, KeyConstants.kSpace)
  , (GLFW.Key'Escape, KeyConstants.kEscape), (GLFW.Key'Enter, KeyConstants.kEnter)
  , (GLFW.Key'Tab, KeyConstants.kTab), (GLFW.Key'F1, KeyConstants.kF1)
  , (GLFW.Key'F2, KeyConstants.kF2), (GLFW.Key'F3, KeyConstants.kF3)
  , (GLFW.Key'F4, KeyConstants.kF4), (GLFW.Key'F5, KeyConstants.kF5)
  , (GLFW.Key'F6, KeyConstants.kF6), (GLFW.Key'F7, KeyConstants.kF7)
  , (GLFW.Key'F8, KeyConstants.kF8), (GLFW.Key'F9, KeyConstants.kF9)
  , (GLFW.Key'F10, KeyConstants.kF10), (GLFW.Key'F11, KeyConstants.kF11)
  , (GLFW.Key'F12, KeyConstants.kF12), (GLFW.Key'Backspace, KeyConstants.kBackspace)
  , (GLFW.Key'Delete, KeyConstants.kDel), (GLFW.Key'Pause, KeyConstants.kPause)
  , (GLFW.Key'LeftShift, KeyConstants.kShift)
  , (GLFW.Key'RightShift, KeyConstants.kShift)
  , (GLFW.Key'LeftControl, KeyConstants.kCtrl)
  , (GLFW.Key'RightControl, KeyConstants.kCtrl)
  , (GLFW.Key'LeftAlt, KeyConstants.kAlt), (GLFW.Key'RightAlt, KeyConstants.kAlt)
  , (GLFW.Key'Insert, KeyConstants.kIns), (GLFW.Key'GraveAccent, ord '`')
  , (GLFW.Key'A, 97), (GLFW.Key'B, 98), (GLFW.Key'C, 99), (GLFW.Key'D, 100)
  , (GLFW.Key'E, 101), (GLFW.Key'F, 102), (GLFW.Key'G, 103), (GLFW.Key'H, 104)
  , (GLFW.Key'I, 105), (GLFW.Key'J, 106), (GLFW.Key'K, 107), (GLFW.Key'L, 108)
  , (GLFW.Key'M, 109), (GLFW.Key'N, 110), (GLFW.Key'O, 111), (GLFW.Key'P, 112)
  , (GLFW.Key'Q, 113), (GLFW.Key'R, 114), (GLFW.Key'S, 115), (GLFW.Key'T, 116)
  , (GLFW.Key'U, 117), (GLFW.Key'V, 118), (GLFW.Key'W, 119), (GLFW.Key'X, 120)
  , (GLFW.Key'Y, 121), (GLFW.Key'Z, 122)
  ]
