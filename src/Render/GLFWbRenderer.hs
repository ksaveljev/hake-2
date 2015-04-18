{-# LANGUAGE OverloadedStrings #-}
module Render.GLFWbRenderer ( glfwbRenderer
                            , glfwbRefExport
                            ) where

import Control.Lens ((^.), use, (.=))
import Control.Monad (when)
import Data.Maybe (isNothing)
import Graphics.UI.GLFW (VideoMode, getPrimaryMonitor, getVideoMode)
import Linear (V3)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Debug.Trace as DT
import qualified Graphics.UI.GLFW as GLFW

import Quake
import QuakeState
import QCommon.XCommandT
import Render.Basic.BasicRenderAPI
import qualified Constants
import qualified Client.VID as VID

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
             , _reGetModeList         = glfwbGetModeList
             , _reGetKeyboardHandler  = Just kbd
             }

glfwbKBD :: KBD
glfwbKBD = DT.trace "GLFWbRenderer.glfwKBD" undefined -- TODO
{-
  KBD { _kbdWinX           :: Int
      , _kbdWinY           :: Int
      , _kbdMX             :: Int
      , _kbdMY             :: Int
      , _kbdInit           :: Quake ()
      , _kbdUpdate         :: Quake ()
      , _kbdClose          :: Quake ()
      , _kbdDoKeyEvent     :: Int -> Bool -> Quake ()
      , _kbdInstallGrabs   :: Quake ()
      , _kbdUninstallGrabs :: Quake ()
      }
-}

glfwbInit :: RenderAPI -> Int -> Int -> Quake Bool
glfwbInit renderAPI vidXPos vidYPos = do
    r <- io $ GLFW.init
    if r
      then do
        -- pre init
        ok <- (renderAPI^.rInit) (glfwbScreenshot renderAPI) (glfwbSetMode) vidXPos vidYPos
        if not ok
          then return False
          -- post init
          else renderAPI^.rInit2

      else do
        VID.printf Constants.printAll "Failed to initialize GLFW-b\n"
        return False

glfwbShutdown :: RenderAPI -> Quake ()
glfwbShutdown renderAPI = renderAPI^.rShutdown

glfwbBeginRegistration :: RenderAPI -> B.ByteString -> Quake ()
glfwbBeginRegistration renderAPI = renderAPI^.rBeginRegistration

glfwbRegisterModel :: RenderAPI -> B.ByteString -> Quake (Maybe ModelT)
glfwbRegisterModel renderAPI = renderAPI^.rRegisterModel

glfwbRegisterSkin :: RenderAPI -> B.ByteString -> Quake (Maybe ImageT)
glfwbRegisterSkin renderAPI = renderAPI^.rRegisterSkin

glfwbRegisterPic :: RenderAPI -> B.ByteString -> Quake (Maybe ImageT)
glfwbRegisterPic renderAPI = renderAPI^.rDrawFindPic

glfwbSetSky :: RenderAPI -> B.ByteString -> Float -> V3 Float -> Quake ()
glfwbSetSky renderAPI = renderAPI^.rSetSky

glfwbEndRegistration :: RenderAPI -> Quake ()
glfwbEndRegistration renderAPI = renderAPI^.rEndRegistration

glfwbRenderFrame :: RenderAPI -> RefDefT -> Quake ()
glfwbRenderFrame renderAPI = renderAPI^.rRenderFrame

glfwbDrawGetPicSize :: RenderAPI -> B.ByteString -> Quake (Maybe (Int, Int))
glfwbDrawGetPicSize renderAPI = renderAPI^.rDrawGetPicSize

glfwbDrawPic :: RenderAPI -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawPic renderAPI = renderAPI^.rDrawPic

glfwbDrawStretchPic :: RenderAPI -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawStretchPic renderAPI = renderAPI^.rDrawStretchPic

glfwbDrawChar :: RenderAPI -> Int -> Int -> Int -> Quake ()
glfwbDrawChar renderAPI = renderAPI^.rDrawChar

glfwbDrawTileClear :: RenderAPI -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawTileClear renderAPI = renderAPI^.rDrawTileClear

glfwbDrawFill :: RenderAPI -> Int -> Int -> Int -> Int -> Int -> Quake ()
glfwbDrawFill renderAPI = renderAPI^.rDrawFill

glfwbDrawFadeScreen :: RenderAPI -> Quake ()
glfwbDrawFadeScreen renderAPI = renderAPI^.rDrawFadeScreen

glfwbDrawStretchRaw :: RenderAPI -> Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
glfwbDrawStretchRaw renderAPI = renderAPI^.rDrawStretchRaw

glfwbCinematicSetPalette :: RenderAPI -> B.ByteString -> Quake ()
glfwbCinematicSetPalette renderAPI = renderAPI^.rSetPalette

glfwbBeginFrame :: RenderAPI -> Float -> Quake ()
glfwbBeginFrame renderAPI = renderAPI^.rBeginFrame

glfwbScreenshot :: RenderAPI -> Quake ()
glfwbScreenshot renderAPI = renderAPI^.glScreenShotF

glfwbEndFrame :: Quake ()
glfwbEndFrame = io (putStrLn "GLFWbRenderer.glfwbEndFrame") >> undefined -- TODO

glfwbAppActivate :: Bool -> Quake ()
glfwbAppActivate _ = return () -- do nothing

glfwbUpdateScreen :: XCommandT -> Quake ()
glfwbUpdateScreen callback = callback

glfwbGetModeList :: Quake (V.Vector VideoMode)
glfwbGetModeList = DT.trace "GLFWbRenderer.glfwbGetModeList" undefined -- TODO

glfwbSetMode :: (Int, Int) -> Int -> Bool -> Quake Int
glfwbSetMode dim mode fullscreen = do
    VID.printf Constants.printAll "Initializing OpenGL display\n"
    VID.printf Constants.printAll $ "...setting mode " `B.append` BC.pack (show mode) `B.append` ":" -- IMPROVE?

    (use $ glfwbGlobals.glfwbOldDisplayMode) >>= \oldMode ->
      when (isNothing oldMode) $ do
        Just monitor <- io $ getPrimaryMonitor
        videoMode <- io $ getVideoMode monitor
        glfwbGlobals.glfwbOldDisplayMode .= videoMode

    io (putStrLn "GLFWbRenderer.glfwbSetMode") >> undefined -- TODO
