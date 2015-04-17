{-# LANGUAGE OverloadedStrings #-}
module Render.DummyRenderer (dummyRenderer) where

import qualified Data.Vector.Unboxed as UV

import Client.RefExportT
import Render.RenderAPI
import Render.Renderer

dummyRenderer :: Renderer
dummyRenderer =
  Renderer { _rName      = "DUMMY"
           , _rRefExport = dummyRefExportT
           , _rRenderAPI = dummyRenderAPI
           , _rKBD       = dummyKBD
           }

dummyRefExportT :: RefExportT
dummyRefExportT =
  RefExportT { _reInit                = (\_ _ -> return False)
             , _reShutDown            = return ()
             , _reBeginRegistration   = (\_ -> return ())
             , _reRegisterModel       = (\_ -> return Nothing)
             , _reRegisterSkin        = (\_ -> return Nothing)
             , _reRegisterPic         = (\_ -> return Nothing)
             , _reSetSky              = (\_ _ _ -> return ())
             , _reEndRegistration     = return ()
             , _reRenderFrame         = (\_ -> return ())
             , _reDrawGetPicSize      = (\_ _ _ -> return ())
             , _reDrawPic             = (\_ _ _ -> return ())
             , _reDrawStretchPic      = (\_ _ _ _ _ -> return ())
             , _reDrawChar            = (\_ _ _ -> return ())
             , _reDrawTileClear       = (\_ _ _ _ _ -> return ())
             , _reDrawFill            = (\_ _ _ _ _ -> return ())
             , _reDrawFadeScreen      = return ()
             , _reDrawStretchRaw      = (\_ _ _ _ _ _ _ -> return ())
             , _reCinematicSetPalette = (\_ -> return ())
             , _reBeginFrame          = (\_ -> return ())
             , _reEndFrame            = return ()
             , _reAppActivate         = (\_ -> return ())
             , _reUpdateScreen        = (\callback -> callback)
             , _reApiVersion          = 0
             , _reGetModeList         = UV.empty
             , _reGetKeyboardHandler  = Nothing
             }

dummyRenderAPI :: RenderAPI
dummyRenderAPI =
    RenderAPI { _rInit              = (\_ _ -> return False)
              , _rInit2             = return False
              , _rShutdown          = return ()
              , _rBeginRegistration = (\_ -> return ())
              , _rRegisterModel     = (\_ -> return Nothing)
              , _rRegisterSkin      = (\_ -> return Nothing)
              , _rDrawFindPic       = (\_ -> return Nothing)
              , _rSetSky            = (\_ _ _ -> return ())
              , _rEndRegistration   = return ()
              , _rRenderFrame       = (\_ -> return ())
              , _rDrawGetPicSize    = (\_ -> return (0, 0))
              , _rDrawPic           = (\_ _ _ -> return ())
              , _rDrawStretchPic    = (\_ _ _ _ _ -> return ())
              , _rDrawChar          = (\_ _ _ -> return ())
              , _rDrawTileClear     = (\_ _ _ _ _ -> return ())
              , _rDrawFill          = (\_ _ _ _ _ -> return ())
              , _rDrawFadeScreen    = return ()
              , _rDrawStretchRaw    = (\_ _ _ _ _ _ _ -> return ())
              , _rSetPalette        = (\_ -> return ())
              , _rBeginFrame        = (\_ -> return ())
              , _glScreenShotF      = return ()
              }

dummyKBD :: KBD
dummyKBD =
  KBD { _kbdWinX           = 0
      , _kbdWinY           = 0
      , _kbdMX             = 0
      , _kbdMY             = 0
      , _kbdInit           = return ()
      , _kbdUpdate         = return ()
      , _kbdClose          = return ()
      , _kbdDoKeyEvent     = (\_ _ -> return ())
      , _kbdInstallGrabs   = return ()
      , _kbdUninstallGrabs = return ()
      }
