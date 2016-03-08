module Render.GLFWbRenderer
  ( glfwbRefExport
  , glfwbRenderer
  ) where

import qualified Client.VIDShared as VID
import qualified Constants
import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import           Render.Basic.BasicRenderAPI (basicRenderAPI)
import           Render.OpenGL.GLDriver
import           Render.OpenGL.GLFWbGLDriver (glfwbGLDriver)
import           Render.RenderAPI
import           Types

import           Control.Lens (use, (^.))
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
glfwbKBDInit = error "GLFWbRenderer.glfwbKBDInit" -- TODO

glfwbKBDUpdate :: Quake ()
glfwbKBDUpdate = error "GLFWbRenderer.glfwbKBDUpdate" -- TODO

glfwbKBDInstallGrabs :: Quake ()
glfwbKBDInstallGrabs = error "GLFWbRenderer.glfwbKBDInstallGrabs" -- TODO

glfwbKBDUninstallGrabs :: Quake ()
glfwbKBDUninstallGrabs = do
    window <- use (glfwbGlobals.glfwbWindow)
    maybe windowError uninstallGrabs window
  where windowError = error "GLFWbRenderer.glfwbKBDUninstallGrabs window is Nothing"
        uninstallGrabs window =
          request (io (GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal))