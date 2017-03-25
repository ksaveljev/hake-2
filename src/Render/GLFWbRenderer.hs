{-# LANGUAGE FlexibleContexts #-}
module Render.GLFWbRenderer
    ( glfwbRefExport
    , glfwbRenderer
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, isEmptyTChan, writeTChan)
import           Control.Lens                 (use, (^.), (.=))
import           Control.Monad                (when, unless)
import           Data.Char                    (ord)
import           Data.Maybe                   (fromMaybe)
import qualified Graphics.UI.GLFW             as GLFW

import qualified Client.Key                   as Key
import qualified Client.KeyConstants          as KeyConstants
import {-# SOURCE #-} qualified Client.VID    as VID
import qualified Constants
import qualified QCommon.CBuf                 as CBuf
import           QCommon.XCommandT            (runXCommandT)
import           QuakeState
import           Render.Basic.BasicRenderAPI  (basicRenderAPI)
import           Render.OpenGL.GLDriver
import           Render.OpenGL.GLFWbGLDriver  (glfwbGLDriver)
import           Render.RenderAPI
import qualified Sys.Timer                    as Timer
import           Types

glfwbRefExport :: RenderAPI -> RefExportT
glfwbRefExport = glfwbRefExportT glfwbKBD

glfwbRenderer :: Renderer
glfwbRenderer =  Renderer
    { _rName      = "GLFWb"
    , _rRefExport = glfwbRefExportT glfwbKBD basicRenderAPI
    }

glfwbRefExportT :: KBD -> RenderAPI -> RefExportT
glfwbRefExportT kbd renderAPI = RefExportT
    { _reInit                = glfwbInit renderAPI
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
glfwbKBD = KBD
    { _kbdInit           = glfwbKBDInit
    , _kbdUpdate         = glfwbKBDUpdate
    , _kbdClose          = return () -- TODO: make sure this is correct
    , _kbdDoKeyEvent     = \_ _ -> error "glfwbKBD.kbdDoKeyEvent" -- TODO
    , _kbdInstallGrabs   = glfwbKBDInstallGrabs
    , _kbdUninstallGrabs = glfwbKBDUninstallGrabs
    }

glfwbKBDInit :: Quake ()
glfwbKBDInit = error "GLFWbRenderer.glfwbKBDInit" -- TODO

glfwbAppActivate :: Bool -> Quake ()
glfwbAppActivate = error "GLFWbRenderer.glfwbAppActivate" -- TODO

glfwbInit :: RenderAPI -> Int -> Int -> Quake Bool
glfwbInit renderAPI vidXPos vidYPos =
    io GLFW.init >>= proceedInit
  where
    proceedInit True =
        (renderAPI^.rInit) glfwbGLDriver vidXPos vidYPos >>= postInit
    proceedInit False = do
        VID.printf Constants.printAll "Failed to initialize GLFW-b\n"
        return False
    postInit False = return False
    postInit True = (renderAPI^.rInit2) glfwbGLDriver

glfwbKBDUpdate :: Quake ()
glfwbKBDUpdate = error "GLFWbRenderer.glfwbKBDUpdate" -- TODO

glfwbKBDInstallGrabs :: Quake ()
glfwbKBDInstallGrabs = error "GLFWbRenderer.glfwbKBDInstallGrabs" -- TODO

glfwbKBDUninstallGrabs :: Quake ()
glfwbKBDUninstallGrabs = error "GLFWbRenderer.glfwbKBDUninstallGrabs" -- TODO