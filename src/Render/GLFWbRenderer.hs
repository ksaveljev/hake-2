{-# LANGUAGE OverloadedStrings #-}
module Render.GLFWbRenderer ( glfwbRenderer
                            , glfwbRefExport
                            ) where

import Control.Lens ((^.), use, (.=), _1, _2, zoom)
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust, isJust)
import Linear (V3)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Debug.Trace as DT
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW

import Quake
import QuakeState
import QCommon.XCommandT
import Render.Basic.BasicRenderAPI
import qualified Constants
import qualified Client.VID as VID
import qualified Render.RenderAPIConstants as RenderAPIConstants

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
glfwbKBD =
  KBD { _kbdInit           = return ()
      , _kbdUpdate         = io (putStrLn "glfwbKBD.kbdUpdate") >> undefined -- TODO
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
        ok <- (renderAPI^.rInit) (glfwbScreenshot renderAPI) (glfwbSetMode) vidXPos vidYPos
        if not ok
          then return False
          -- post init
          else (renderAPI^.rInit2) glfwbSetSwapInterval endFrame

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

glfwbGetModeList :: Quake (V.Vector GLFW.VideoMode)
glfwbGetModeList = do
    Just oldMode <- use $ glfwbGlobals.glfwbOldDisplayMode

    Just monitor <- io $ GLFW.getPrimaryMonitor
    vm <- io $ GLFW.getVideoModes monitor

    case vm of
      Nothing -> return $ V.fromList [oldMode]
      Just modes -> do
        -- TODO: implement this stuff:
        {-
        int j = 0;
        DisplayMode ml = null;
        for (j = 0; j < l.size(); j++) {
                ml = (DisplayMode)l.get(j);
                if (ml.getWidth() > m.getWidth()) break;
                if (ml.getWidth() == m.getWidth() && ml.getHeight() >= m.getHeight()) break;
        }
        if (j == l.size()) {
                l.addLast(m);
        } else if (ml.getWidth() > m.getWidth() || ml.getHeight() > m.getHeight()) {
                l.add(j, m);
        } else if (m.getRefreshRate() > ml.getRefreshRate()){
                l.remove(j);
                l.add(j, m);
        }
        -}
        return $ V.fromList $ filter (validDisplayMode oldMode) modes

  where validDisplayMode :: GLFW.VideoMode -> GLFW.VideoMode -> Bool
        validDisplayMode oldMode newMode =
          GLFW.videoModeRedBits oldMode == GLFW.videoModeRedBits newMode &&
          GLFW.videoModeGreenBits oldMode == GLFW.videoModeGreenBits newMode &&
          GLFW.videoModeBlueBits oldMode == GLFW.videoModeBlueBits newMode &&
          GLFW.videoModeRefreshRate oldMode >= GLFW.videoModeRefreshRate newMode &&
          GLFW.videoModeWidth newMode >= 320 &&
          GLFW.videoModeHeight newMode >= 240

-- first param is dim but it is not used anywhere for some reason
glfwbSetMode :: (Int, Int) -> Int -> Bool -> Quake Int
glfwbSetMode _ mode fullscreen = do
    VID.printf Constants.printAll "Initializing OpenGL display\n"
    VID.printf Constants.printAll $ "...setting mode " `B.append` BC.pack (show mode) `B.append` ":" -- IMPROVE?

    (use $ glfwbGlobals.glfwbOldDisplayMode) >>= \oldMode ->
      when (isNothing oldMode) $ do
        Just monitor <- io $ GLFW.getPrimaryMonitor
        videoMode <- io $ GLFW.getVideoMode monitor
        glfwbGlobals.glfwbOldDisplayMode .= videoMode

    ok <- VID.getModeInfo mode
    case ok of
      Nothing -> do
        VID.printf Constants.printAll " invalid mode\n"
        return RenderAPIConstants.rsErrInvalidMode
      Just newDim -> do
        VID.printf Constants.printAll $ BC.pack (show newDim)

        -- destroy the existing window
        shutdown

        -- TODO: handle errors (Nothing)
        Just monitor <- io $ GLFW.getPrimaryMonitor

        if fullscreen
          then do
            -- TODO: handle errors (Nothing)
            Just window <- io $ GLFW.createWindow (newDim^._1) (newDim^._2) "Hake2 (GLFWb)" (Just monitor) Nothing
            glfwbGlobals.glfwbWindow .= Just window
          else do
            -- TODO: handle errors (Nothing)
            Just window <- io $ GLFW.createWindow (newDim^._1) (newDim^._2) "Hake2 (GLFWb)" Nothing Nothing
            glfwbGlobals.glfwbWindow .= Just window

        Just currentMode <- io $ GLFW.getVideoMode monitor
        when fullscreen $
          VID.printf Constants.printAll ("...setting fullscreen " `B.append` getModeString currentMode `B.append` "\n")

        let newWidth = GLFW.videoModeWidth currentMode
            newHeight = GLFW.videoModeHeight currentMode

        setVid newWidth newHeight

        -- let the sound and input subsystems know about the new window
        VID.newWindow newWidth newHeight

        return RenderAPIConstants.rsErrOk

shutdown :: Quake ()
shutdown =
    (use $ glfwbGlobals.glfwbWindow) >>= \w ->
      when (isJust w) $ do
        io (GLFW.destroyWindow (fromJust w))
        glfwbGlobals.glfwbWindow .= Nothing

getModeString :: GLFW.VideoMode -> B.ByteString
getModeString vm =
    BC.pack (show $ GLFW.videoModeWidth vm) `B.append`
    "x" `B.append`
    BC.pack (show $ GLFW.videoModeHeight vm) `B.append`
    "x" `B.append`
    BC.pack (show $ GLFW.videoModeRedBits vm) `B.append` -- TODO: ensure this is what we think it is
    "@" `B.append`
    BC.pack (show $ GLFW.videoModeRefreshRate vm) `B.append`
    "Hz"

setVid :: Int -> Int -> Quake ()
setVid width height =
    zoom (fastRenderAPIGlobals.frVid) $ do
      vdNewWidth .= width
      vdNewHeight .= height

endFrame :: Quake ()
endFrame = do
    GL.glFlush
    Just window <- use $ glfwbGlobals.glfwbWindow
    io $ GLFW.swapBuffers window

glfwbSetSwapInterval :: Int -> Quake ()
glfwbSetSwapInterval v = io $ GLFW.swapInterval v
