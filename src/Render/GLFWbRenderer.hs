{-# LANGUAGE OverloadedStrings #-}
module Render.GLFWbRenderer ( glfwbRenderer
                            , glfwbRefExport
                            ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, isEmptyTChan)
import Control.Lens ((^.), use, (.=))
import Control.Monad (when, unless, void)
import Linear (V3)
import qualified Data.ByteString as B
import qualified Graphics.UI.GLFW as GLFW

import Quake
import QuakeState
import QCommon.XCommandT
import Render.Basic.BasicRenderAPI
import Render.OpenGL.GLFWbGLDriver
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.CBuf as CBuf

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
      , _kbdClose          = io (putStrLn "glfwbKBD.kbdUpdate") >> undefined -- TODO
      , _kbdDoKeyEvent     = (\_ _ -> io (putStrLn "glfwbKBD.kbdDoKeyEvent") >> undefined) -- TODO
      , _kbdInstallGrabs   = io (putStrLn "glfwbKBD.kbdUpdate") >> undefined -- TODO
      , _kbdUninstallGrabs = io (putStrLn "glfwbKBD.kbdUpdate") >> undefined -- TODO
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

glfwbRegisterModel :: RenderAPI -> B.ByteString -> Quake (Maybe ModelReference)
glfwbRegisterModel renderAPI = (renderAPI^.rRegisterModel) glfwbGLDriver

glfwbRegisterSkin :: RenderAPI -> B.ByteString -> Quake (Maybe ImageReference)
glfwbRegisterSkin renderAPI = (renderAPI^.rRegisterSkin) glfwbGLDriver

glfwbRegisterPic :: RenderAPI -> B.ByteString -> Quake (Maybe ImageReference)
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
glfwbUpdateScreen callback = callback

glfwbKBDInit :: Quake ()
glfwbKBDInit = do
    Just window <- use $ glfwbGlobals.glfwbWindow

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
            void $ case msg of
                     KeyPress -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
                     KeyRelease -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
                     MotionNotify -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
                     ButtonPress -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
                     ButtonRelease -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
                     ConfigureNotify -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO
                     WheelMoved -> io (putStrLn "GLFWbRenderer.glfwbKBDUpdate#handleEvents") >> undefined -- TODO

            handleEvents kbdChan

keyCallback :: TChan GLFWKBDEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback _ _ _ _ _ _ = putStrLn "GLFWbRenderer.keyCallback" >> undefined -- TODO

mouseButtonCallback :: TChan GLFWKBDEvent -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback _ _ _ _ _ = putStrLn "GLFWbRenderer.mouseButtonCallback" >> undefined -- TODO

cursorPosCallback :: TChan GLFWKBDEvent -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback _ _ _ _ = putStrLn "GLFWbRenderer.cursorPosCallback" >> undefined -- TODO

scrollCallback :: TChan GLFWKBDEvent -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback _ _ _ _ = putStrLn "GLFWbRenderer.scrollCallback" >> undefined -- TODO
