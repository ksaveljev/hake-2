{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.GLFWbRenderer ( glfwbRenderer
                            , glfwbRefExport
                            ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, isEmptyTChan, writeTChan)
import Control.Lens ((^.), use, (.=))
import Control.Monad (when, unless, void)
import Data.Char (ord)
import Data.IORef (IORef)
import Linear (V3)
import qualified Data.ByteString as B
import qualified Graphics.UI.GLFW as GLFW

import Types
import QuakeState
import QCommon.XCommandT
import Render.Basic.BasicRenderAPI
import Render.OpenGL.GLFWbGLDriver
import qualified Constants
import qualified Client.Key as Key
import qualified Client.KeyConstants as KeyConstants
import qualified Client.VID as VID
import qualified QCommon.CBuf as CBuf
import qualified Sys.Timer as Timer

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
             , _reShutDown            = glfwbShutdown renderAPI
             , _reBeginRegistration   = glfwbBeginRegistration renderAPI
             , _reRegisterModel       = glfwbRegisterModel renderAPI
             , _reRegisterSkin        = glfwbRegisterSkin renderAPI
             , _reRegisterPic         = glfwbRegisterPic renderAPI
             , _reSetSky              = glfwbSetSky renderAPI
             , _reEndRegistration     = glfwbEndRegistration renderAPI
             , _reRenderFrame         = glfwbRenderFrame renderAPI
             , _reDrawGetPicSize      = glfwbDrawGetPicSize renderAPI
             , _reDrawPic             = glfwbDrawPic renderAPI
             , _reDrawStretchPic      = glfwbDrawStretchPic renderAPI
             , _reDrawChar            = glfwbDrawChar renderAPI
             , _reDrawTileClear       = glfwbDrawTileClear renderAPI
             , _reDrawFill            = glfwbDrawFill renderAPI
             , _reDrawFadeScreen      = glfwbDrawFadeScreen renderAPI
             , _reDrawStretchRaw      = glfwbDrawStretchRaw renderAPI
             , _reCinematicSetPalette = glfwbCinematicSetPalette renderAPI
             , _reBeginFrame          = glfwbBeginFrame renderAPI
             , _reEndFrame            = glfwbEndFrame
             , _reAppActivate         = glfwbAppActivate
             , _reUpdateScreen        = glfwbUpdateScreen
             , _reApiVersion          = Constants.apiVersion
             , _reGetModeList         = glfwbGLDriver^.gldGetModeList
             , _reGetKeyboardHandler  = kbd
             }

glfwbKBD :: KBD
glfwbKBD =
  KBD { _kbdInit           = glfwbKBDInit
      , _kbdUpdate         = glfwbKBDUpdate
      , _kbdClose          = return () -- TODO: make sure this is correct
      , _kbdDoKeyEvent     = (\_ _ -> io (putStrLn "glfwbKBD.kbdDoKeyEvent") >> undefined) -- TODO
      , _kbdInstallGrabs   = glfwbKBDInstallGrabs
      , _kbdUninstallGrabs = glfwbKBDUninstallGrabs
      }

glfwbInit :: RenderAPI -> Int -> Int -> Quake Bool
glfwbInit renderAPI vidXPos vidYPos = do
    r <- io $ GLFW.init
    if r
      then do
        -- pre init
        ok <- (renderAPI^.rInit) glfwbGLDriver vidXPos vidYPos
        if not ok
          then return False
          -- post init
          else (renderAPI^.rInit2) glfwbGLDriver

      else do
        VID.printf Constants.printAll "Failed to initialize GLFW-b\n"
        return False

glfwbShutdown :: RenderAPI -> Quake ()
glfwbShutdown renderAPI = (renderAPI^.rShutdown) glfwbGLDriver

glfwbBeginRegistration :: RenderAPI -> B.ByteString -> Quake ()
glfwbBeginRegistration renderAPI = (renderAPI^.rBeginRegistration) glfwbGLDriver

glfwbRegisterModel :: RenderAPI -> B.ByteString -> Quake (Maybe (IORef ModelT))
glfwbRegisterModel renderAPI = (renderAPI^.rRegisterModel) glfwbGLDriver

glfwbRegisterSkin :: RenderAPI -> B.ByteString -> Quake (Maybe (IORef ImageT))
glfwbRegisterSkin renderAPI = (renderAPI^.rRegisterSkin) glfwbGLDriver

glfwbRegisterPic :: RenderAPI -> B.ByteString -> Quake (Maybe (IORef ImageT))
glfwbRegisterPic renderAPI = (renderAPI^.rDrawFindPic) glfwbGLDriver

glfwbSetSky :: RenderAPI -> B.ByteString -> Float -> V3 Float -> Quake ()
glfwbSetSky renderAPI = (renderAPI^.rSetSky) glfwbGLDriver

glfwbEndRegistration :: RenderAPI -> Quake ()
glfwbEndRegistration renderAPI = (renderAPI^.rEndRegistration) glfwbGLDriver

glfwbRenderFrame :: RenderAPI -> RefDefT -> Quake ()
glfwbRenderFrame renderAPI = (renderAPI^.rRenderFrame) glfwbGLDriver

glfwbDrawGetPicSize :: RenderAPI -> B.ByteString -> Quake (Maybe (Int, Int))
glfwbDrawGetPicSize renderAPI = (renderAPI^.rDrawGetPicSize) glfwbGLDriver

glfwbDrawPic :: RenderAPI -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawPic renderAPI = (renderAPI^.rDrawPic) glfwbGLDriver

glfwbDrawStretchPic :: RenderAPI -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawStretchPic renderAPI = (renderAPI^.rDrawStretchPic) glfwbGLDriver

glfwbDrawChar :: RenderAPI -> Int -> Int -> Int -> Quake ()
glfwbDrawChar renderAPI = (renderAPI^.rDrawChar) glfwbGLDriver

glfwbDrawTileClear :: RenderAPI -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawTileClear renderAPI = (renderAPI^.rDrawTileClear) glfwbGLDriver

glfwbDrawFill :: RenderAPI -> Int -> Int -> Int -> Int -> Int -> Quake ()
glfwbDrawFill renderAPI = (renderAPI^.rDrawFill) glfwbGLDriver

glfwbDrawFadeScreen :: RenderAPI -> Quake ()
glfwbDrawFadeScreen renderAPI = (renderAPI^.rDrawFadeScreen) glfwbGLDriver

glfwbDrawStretchRaw :: RenderAPI -> Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawStretchRaw renderAPI = (renderAPI^.rDrawStretchRaw) glfwbGLDriver

glfwbCinematicSetPalette :: RenderAPI -> Maybe B.ByteString -> Quake ()
glfwbCinematicSetPalette renderAPI = (renderAPI^.rSetPalette) glfwbGLDriver

glfwbBeginFrame :: RenderAPI -> Float -> Quake ()
glfwbBeginFrame renderAPI = (renderAPI^.rBeginFrame) glfwbGLDriver

glfwbEndFrame :: Quake ()
glfwbEndFrame = glfwbGLDriver^.gldEndFrame

glfwbAppActivate :: Bool -> Quake ()
glfwbAppActivate _ = io (putStrLn "GLFWbRenderer.appActivate") >> undefined -- TODO

glfwbUpdateScreen :: XCommandT -> Quake ()
glfwbUpdateScreen callback = callback^.xcCmd

glfwbKBDInit :: Quake ()
glfwbKBDInit = do
    Just window <- use $ glfwbGlobals.glfwbWindow

    (w, h) <- io $ GLFW.getWindowSize window

    -- TODO: make sure this is a correct spot to init these values
    -- maybe do it in ConfigureNotify section?
    kbdGlobals.kbdWinW2 .= w `div` 2
    kbdGlobals.kbdWinH2 .= h `div` 2

    kbdChan <- io (newTChanIO :: IO (TChan GLFWKBDEvent))
    glfwbGlobals.glfwbKBDChan .= Just kbdChan

    io $ GLFW.setKeyCallback window (Just $ keyCallback kbdChan)
    io $ GLFW.setMouseButtonCallback window (Just $ mouseButtonCallback kbdChan)
    io $ GLFW.setCursorPosCallback window (Just $ cursorPosCallback kbdChan)
    io $ GLFW.setScrollCallback window (Just $ scrollCallback kbdChan)

glfwbKBDUpdate :: Quake ()
glfwbKBDUpdate = do
    io $ GLFW.pollEvents

    Just window <- use $ glfwbGlobals.glfwbWindow
    io (GLFW.windowShouldClose window) >>= \quit ->
      when quit $
        CBuf.executeText Constants.execAppend "quit"

    Just kbdChan <- use $ glfwbGlobals.glfwbKBDChan

    handleEvents kbdChan

  where handleEvents :: TChan GLFWKBDEvent -> Quake ()
        handleEvents kbdChan = do
          emptyChan <- io $ atomically $ isEmptyTChan kbdChan

          unless emptyChan $ do
            msg <- io $ atomically $ readTChan kbdChan

            case msg of
              KeyPress key -> doKeyEvent (xLateKey key) True
              KeyRelease key -> doKeyEvent (xLateKey key) False
              CursorPosition x' y' -> do
                x <- use $ kbdGlobals.kbdX
                y <- use $ kbdGlobals.kbdY

                kbdGlobals.kbdMx .= (truncate x' - x) * 2
                kbdGlobals.kbdMy .= (truncate y' - y) * 2

                kbdGlobals.kbdX .= truncate x'
                kbdGlobals.kbdY .= truncate y'

              MouseButtonPress button -> doKeyEvent (mouseEventToKey button) True
              MouseButtonRelease button -> doKeyEvent (mouseEventToKey button) False
              MouseWheelScroll scroll -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
              ConfigureNotify -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO

            handleEvents kbdChan

keyCallback :: TChan GLFWKBDEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback kbdChan _ key _ action _ =
    if | action == GLFW.KeyState'Pressed -> atomically $ writeTChan kbdChan (KeyPress key)
       | action == GLFW.KeyState'Released -> atomically $ writeTChan kbdChan (KeyRelease key)
       | otherwise -> return ()

mouseButtonCallback :: TChan GLFWKBDEvent -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback kbdChan _ button action _ =
    if | action == GLFW.MouseButtonState'Pressed -> atomically $ writeTChan kbdChan (MouseButtonPress button)
       | action == GLFW.MouseButtonState'Released -> atomically $ writeTChan kbdChan (MouseButtonRelease button)
       | otherwise -> return ()

cursorPosCallback :: TChan GLFWKBDEvent -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback kbdChan _ xPos yPos =
    atomically $ writeTChan kbdChan (CursorPosition xPos yPos)

scrollCallback :: TChan GLFWKBDEvent -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback kbdChan _ _ yOffset =
    -- A simple mouse wheel, being vertical, provides offsets along the Y-axis.
    atomically$ writeTChan kbdChan (MouseWheelScroll yOffset)

glfwbKBDInstallGrabs :: Quake ()
glfwbKBDInstallGrabs = do
    Just window <- use $ glfwbGlobals.glfwbWindow
    io $ GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
    (x, y) <- io $ GLFW.getCursorPos window
    kbdGlobals.kbdX .= truncate x
    kbdGlobals.kbdY .= truncate y

glfwbKBDUninstallGrabs :: Quake ()
glfwbKBDUninstallGrabs = do
    Just window <- use $ glfwbGlobals.glfwbWindow
    io $ GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal

-- TODO: what about keypad left/right/up/down?
xLateKey :: GLFW.Key -> Int
xLateKey key =
    if | key == GLFW.Key'PageUp       -> KeyConstants.kPgUp
       | key == GLFW.Key'PageDown     -> KeyConstants.kPgDn
       | key == GLFW.Key'Home         -> KeyConstants.kHome
       | key == GLFW.Key'End          -> KeyConstants.kEnd
       | key == GLFW.Key'Left         -> KeyConstants.kLeftArrow
       | key == GLFW.Key'Right        -> KeyConstants.kRightArrow
       | key == GLFW.Key'Down         -> KeyConstants.kDownArrow
       | key == GLFW.Key'Up           -> KeyConstants.kUpArrow
       | key == GLFW.Key'Space        -> KeyConstants.kSpace
       | key == GLFW.Key'Escape       -> KeyConstants.kEscape
       | key == GLFW.Key'Enter        -> KeyConstants.kEnter
       | key == GLFW.Key'Tab          -> KeyConstants.kTab
       | key == GLFW.Key'F1           -> KeyConstants.kF1
       | key == GLFW.Key'F2           -> KeyConstants.kF2 
       | key == GLFW.Key'F3           -> KeyConstants.kF3
       | key == GLFW.Key'F4           -> KeyConstants.kF4
       | key == GLFW.Key'F5           -> KeyConstants.kF5
       | key == GLFW.Key'F6           -> KeyConstants.kF6
       | key == GLFW.Key'F7           -> KeyConstants.kF7
       | key == GLFW.Key'F8           -> KeyConstants.kF8
       | key == GLFW.Key'F9           -> KeyConstants.kF9
       | key == GLFW.Key'F10          -> KeyConstants.kF10
       | key == GLFW.Key'F11          -> KeyConstants.kF11
       | key == GLFW.Key'F12          -> KeyConstants.kF12
       | key == GLFW.Key'Backspace    -> KeyConstants.kBackspace
       | key == GLFW.Key'Delete       -> KeyConstants.kDel
       | key == GLFW.Key'Pause        -> KeyConstants.kPause
       | key == GLFW.Key'LeftShift    -> KeyConstants.kShift
       | key == GLFW.Key'RightShift   -> KeyConstants.kShift
       | key == GLFW.Key'LeftControl  -> KeyConstants.kCtrl
       | key == GLFW.Key'RightControl -> KeyConstants.kCtrl
       | key == GLFW.Key'LeftAlt      -> KeyConstants.kAlt
       | key == GLFW.Key'RightAlt     -> KeyConstants.kAlt
       | key == GLFW.Key'Insert       -> KeyConstants.kIns
       | key == GLFW.Key'GraveAccent  -> ord '`'
       | key == GLFW.Key'A            -> 97   -- a
       | key == GLFW.Key'B            -> 98   -- b
       | key == GLFW.Key'C            -> 99   -- c
       | key == GLFW.Key'D            -> 100  -- d
       | key == GLFW.Key'E            -> 101  -- e
       | key == GLFW.Key'F            -> 102  -- f
       | key == GLFW.Key'G            -> 103  -- g
       | key == GLFW.Key'H            -> 104  -- h
       | key == GLFW.Key'I            -> 105  -- i
       | key == GLFW.Key'J            -> 106  -- j
       | key == GLFW.Key'K            -> 107  -- k
       | key == GLFW.Key'L            -> 108  -- l
       | key == GLFW.Key'M            -> 109  -- m
       | key == GLFW.Key'N            -> 110  -- n
       | key == GLFW.Key'O            -> 111  -- o
       | key == GLFW.Key'P            -> 112  -- p
       | key == GLFW.Key'Q            -> 113  -- q
       | key == GLFW.Key'R            -> 114  -- r
       | key == GLFW.Key'S            -> 115  -- s
       | key == GLFW.Key'T            -> 116  -- t
       | key == GLFW.Key'U            -> 117  -- u
       | key == GLFW.Key'V            -> 118  -- v
       | key == GLFW.Key'W            -> 119  -- w
       | key == GLFW.Key'X            -> 120  -- x
       | key == GLFW.Key'Y            -> 121  -- y
       | key == GLFW.Key'Z            -> 122  -- z
       | otherwise                    -> 0

-- BUTTON1(left) BUTTON2(center) BUTTON3(right)
-- K_MOUSE1      K_MOUSE3        K_MOUSE2
mouseEventToKey :: GLFW.MouseButton -> Int
mouseEventToKey button =
    if | button == GLFW.MouseButton'3 -> KeyConstants.kMouse3
       | button == GLFW.MouseButton'2 -> KeyConstants.kMouse2
       | otherwise -> KeyConstants.kMouse1

doKeyEvent :: Int -> Bool -> Quake ()
doKeyEvent key down = do
    ms <- Timer.milliseconds
    Key.event key down ms
