{-# LANGUAGE OverloadedStrings #-}
module Render.DummyRenderer (dummyRenderer) where

import qualified Data.Vector as V

import Client.RefExportT
import Render.RenderAPI
import Render.Renderer

dummyRenderer :: Renderer
dummyRenderer =
  Renderer { _rName      = "DUMMY"
           , _rRefExport = dummyRefExportT dummyRenderAPI
           }

dummyRefExportT :: RenderAPI -> RefExportT
dummyRefExportT _ =
  RefExportT { _reInit                = (\_ _ -> return False)
             , _reShutDown            = return ()
             , _reBeginRegistration   = (\_ -> return ())
             , _reRegisterModel       = (\_ -> return Nothing)
             , _reRegisterSkin        = (\_ -> return Nothing)
             , _reRegisterPic         = (\_ -> return Nothing)
             , _reSetSky              = (\_ _ _ -> return ())
             , _reEndRegistration     = return ()
             , _reRenderFrame         = (\_ -> return ())
             , _reDrawGetPicSize      = (\_ -> return Nothing)
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
             , _reGetModeList         = return V.empty
             , _reGetKeyboardHandler  = dummyKBD
             }

dummyKBD :: KBD
dummyKBD =
  KBD { _kbdInit           = return ()
      , _kbdUpdate         = return ()
      , _kbdClose          = return ()
      , _kbdDoKeyEvent     = \_ _ -> return ()
      , _kbdInstallGrabs   = return ()
      , _kbdUninstallGrabs = return ()
      }

dummyRenderAPI :: RenderAPI
dummyRenderAPI =
    RenderAPI { _rInit              = \_ _ _ -> return False
              , _rInit2             = \_ -> return False
              , _rShutdown          = \_ -> return ()
              , _rBeginRegistration = \_ _ -> return ()
              , _rRegisterModel     = \_ _ -> return Nothing
              , _rRegisterSkin      = \_ _ -> return Nothing
              , _rDrawFindPic       = \_ _ -> return Nothing
              , _rSetSky            = \_ _ _ _ -> return ()
              , _rEndRegistration   = \_ -> return ()
              , _rRenderFrame       = \_ _ -> return ()
              , _rDrawGetPicSize    = \_ _ -> return Nothing
              , _rDrawPic           = \_ _ _ _ -> return ()
              , _rDrawStretchPic    = \_ _ _ _ _ _ -> return ()
              , _rDrawChar          = \_ _ _ _ -> return ()
              , _rDrawTileClear     = \_ _ _ _ _ _ -> return ()
              , _rDrawFill          = \_ _ _ _ _ _ -> return ()
              , _rDrawFadeScreen    = \_ -> return ()
              , _rDrawStretchRaw    = \_ _ _ _ _ _ _ _ -> return ()
              , _rSetPalette        = \_ _ -> return ()
              , _rBeginFrame        = \_ _ -> return ()
              , _glScreenShotF      = \_ -> return ()
              }
