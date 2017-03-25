module Render.DummyRenderer
    ( dummyRefExport
    , dummyRenderer
    , dummyRenderAPI
    ) where

import           Control.Lens      ((^.))
import qualified Data.Vector       as V

import qualified Constants
import           QCommon.XCommandT (runXCommandT)
import           Render.RenderAPI
import           Types

dummyRenderer :: Renderer
dummyRenderer = Renderer
    { _rName      = "DUMMY"
    , _rRefExport = dummyRefExportT dummyKBD dummyRenderAPI
    }
           
dummyRefExport :: RenderAPI -> RefExportT
dummyRefExport = dummyRefExportT dummyKBD

dummyRefExportT :: KBD -> RenderAPI -> RefExportT
dummyRefExportT kbd renderAPI = RefExportT
    { _reInit                = dummyInit renderAPI
    , _reShutDown            = (renderAPI^.rShutdown) dummyGLDriver
    , _reBeginRegistration   = (renderAPI^.rBeginRegistration) dummyGLDriver
    , _reRegisterModel       = (renderAPI^.rRegisterModel) dummyGLDriver
    , _reRegisterSkin        = (renderAPI^.rRegisterSkin) dummyGLDriver
    , _reRegisterPic         = (renderAPI^.rDrawFindPic) dummyGLDriver
    , _reSetSky              = (renderAPI^.rSetSky) dummyGLDriver
    , _reEndRegistration     = (renderAPI^.rEndRegistration) dummyGLDriver
    , _reRenderFrame         = (renderAPI^.rRenderFrame) dummyGLDriver
    , _reDrawGetPicSize      = (renderAPI^.rDrawGetPicSize) dummyGLDriver
    , _reDrawPic             = (renderAPI^.rDrawPic) dummyGLDriver
    , _reDrawStretchPic      = (renderAPI^.rDrawStretchPic) dummyGLDriver
    , _reDrawChar            = (renderAPI^.rDrawChar) dummyGLDriver
    , _reDrawTileClear       = (renderAPI^.rDrawTileClear) dummyGLDriver
    , _reDrawFill            = (renderAPI^.rDrawFill) dummyGLDriver
    , _reDrawFadeScreen      = (renderAPI^.rDrawFadeScreen) dummyGLDriver
    , _reDrawStretchRaw      = (renderAPI^.rDrawStretchRaw) dummyGLDriver
    , _reCinematicSetPalette = (renderAPI^.rSetPalette) dummyGLDriver
    , _reBeginFrame          = (renderAPI^.rBeginFrame) dummyGLDriver
    , _reEndFrame            = return ()
    , _reAppActivate         = \_ -> return ()
    , _reUpdateScreen        = runXCommandT
    , _reApiVersion          = Constants.apiVersion
    , _reGetModeList         = return (V.fromList [DummyVideoMode])
    , _reGetKeyboardHandler  = kbd
    }
             
dummyInit :: RenderAPI -> Int -> Int -> Quake Bool
dummyInit renderAPI vidXPos vidYPos =
    (renderAPI^.rInit) dummyGLDriver vidXPos vidYPos >>= postInit
  where
    postInit False = return False
    postInit True = (renderAPI^.rInit2) dummyGLDriver

dummyKBD :: KBD
dummyKBD = KBD
    { _kbdInit           = return ()
    , _kbdUpdate         = return ()
    , _kbdClose          = return ()
    , _kbdDoKeyEvent     = \_ _ -> return ()
    , _kbdInstallGrabs   = return ()
    , _kbdUninstallGrabs = return ()
    }

dummyRenderAPI :: RenderAPI
dummyRenderAPI = RenderAPI
    { _rInit              = \_ _ _ -> return True
    , _rInit2             = \_ -> return True
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

dummyGLDriver :: GLDriver
dummyGLDriver = GLDriver
    { _gldInit            = \_ _ -> return True
    , _gldSetMode         = \_ _ _ -> return 0
    , _gldShutdown        = return ()
    , _gldBeginFrame      = \_ -> return ()
    , _gldEndFrame        = return ()
    , _gldAppActivate     = \_ -> return ()
    , _gldEnableLogging   = \_ -> return ()
    , _gldLogNewFrame     = return ()
    , _gldGetModeList     = return V.empty
    , _gldUpdateScreen    = runXCommandT
    , _gldSetSwapInterval = \_ -> return ()
    }