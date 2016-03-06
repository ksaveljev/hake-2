module Render.GLFWbRenderer
  ( glfwbRefExport
  , glfwbRenderer
  ) where

import qualified Constants
import           QCommon.XCommandT (runXCommandT)
import           Render.Basic.BasicRenderAPI (basicRenderAPI)
import           Render.OpenGL.GLDriver
import           Render.OpenGL.GLFWbGLDriver (glfwbGLDriver)
import           Render.RenderAPI
import           Types

import           Control.Lens ((^.))
import qualified Data.ByteString as B
import           Data.IORef (IORef)
import           Linear (V3)

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
glfwbKBD = error "Render.glfwbKBD"

glfwbInit :: RenderAPI -> Int -> Int -> Quake Bool
glfwbInit = error "GLFWbRenderer.glfwbInit" -- TODO

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
glfwbAppActivate = error "GLFWbRenderer.glfwbAppActivate" -- TODO

glfwbUpdateScreen :: XCommandT -> Quake ()
glfwbUpdateScreen = runXCommandT
