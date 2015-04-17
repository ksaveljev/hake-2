{-# LANGUAGE OverloadedStrings #-}
module Render.GLFWbRenderer ( glfwbRenderer
                            , glfwbRefExport
                            ) where

import Control.Lens ((^.))
import qualified Debug.Trace as DT

import Quake
import Render.Basic.BasicRenderAPI
import Render.Renderer

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
             , _reShutDown            = DT.trace "_reShutDown" undefined -- TODO
             , _reBeginRegistration   = DT.trace "_reBeginRegistration" undefined -- TODO
             , _reRegisterModel       = DT.trace "_reRegisterModel" undefined -- TODO
             , _reRegisterSkin        = DT.trace "_reRegisterSkin" undefined -- TODO
             , _reRegisterPic         = DT.trace "_reRegisterPic" undefined -- TODO
             , _reSetSky              = DT.trace "_reSetSky" undefined -- TODO
             , _reEndRegistration     = DT.trace "_reEndRegistration" undefined -- TODO
             , _reRenderFrame         = DT.trace "_reRenderFrame" undefined -- TODO
             , _reDrawGetPicSize      = DT.trace "_reDrawGetPicSize" undefined -- TODO
             , _reDrawPic             = DT.trace "_reDrawPic" undefined -- TODO
             , _reDrawStretchPic      = DT.trace "_reDrawStretchPic" undefined -- TODO
             , _reDrawChar            = DT.trace "_reDrawChar" undefined -- TODO
             , _reDrawTileClear       = DT.trace "_reDrawTileClear" undefined -- TODO
             , _reDrawFill            = DT.trace "_reDrawFill" undefined -- TODO
             , _reDrawFadeScreen      = DT.trace "_reDrawFadeScreen" undefined -- TODO
             , _reDrawStretchRaw      = DT.trace "_reDrawStretchRaw" undefined -- TODO
             , _reCinematicSetPalette = DT.trace "_reCinematicSetPalette" undefined -- TODO
             , _reBeginFrame          = DT.trace "_reBeginFrame" undefined -- TODO
             , _reEndFrame            = DT.trace "_reEndFrame" undefined -- TODO
             , _reAppActivate         = DT.trace "_reAppActivate" undefined -- TODO
             , _reUpdateScreen        = DT.trace "_reUpdateScreen" undefined -- TODO
             , _reApiVersion          = DT.trace "_reApiVersion" undefined -- TODO
             , _reGetModeList         = DT.trace "_reGetModeList" undefined -- TODO
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
    -- pre init
    ok <- (renderAPI^.rInit) vidXPos vidYPos
    if not ok
      then return False
      -- post init
      else renderAPI^.rInit2
