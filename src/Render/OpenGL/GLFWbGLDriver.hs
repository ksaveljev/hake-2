{-# LANGUAGE OverloadedStrings #-}
module Render.OpenGL.GLFWbGLDriver ( glfwbGLDriver
                                   , gldInit
                                   , gldSetMode
                                   , gldShutdown
                                   , gldBeginFrame
                                   , gldEndFrame
                                   , gldAppActivate
                                   , gldEnableLogging
                                   , gldLogNewFrame
                                   , gldGetModeList
                                   , gldUpdateScreen
                                   , gldSetSwapInterval
                                   ) where

import Control.Lens (use, (.=), (^.), zoom, _1, _2)
import Control.Monad (when)
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW

import Quake
import QuakeState
import QCommon.XCommandT
import Render.OpenGL.GLDriver
import Render.VideoMode
import qualified Constants
import qualified Client.VID as VID
import qualified Render.RenderAPIConstants as RenderAPIConstants

glfwbGLDriver :: GLDriver
glfwbGLDriver =
  GLDriver { _gldInit            = glDriverInit
           , _gldSetMode         = glDriverSetMode
           , _gldShutdown        = glDriverShutdown
           , _gldBeginFrame      = glDriverBeginFrame
           , _gldEndFrame        = glDriverEndFrame
           , _gldAppActivate     = glDriverAppActivate
           , _gldEnableLogging   = glDriverEnableLogging
           , _gldLogNewFrame     = glDriverLogNewFrame
           , _gldGetModeList     = glDriverGetModeList
           , _gldUpdateScreen    = glDriverUpdateScreen
           , _gldSetSwapInterval = glDriverSetSwapInterval
           }

glDriverSetSwapInterval :: Int -> Quake ()
glDriverSetSwapInterval v = io $ GLFW.swapInterval v

glDriverGetModeList :: Quake (V.Vector VideoMode)
glDriverGetModeList = do
    Just oldMode <- use $ glfwbGlobals.glfwbOldDisplayMode

    Just monitor <- io $ GLFW.getPrimaryMonitor
    vm <- io $ GLFW.getVideoModes monitor

    case vm of
      Nothing -> return $ V.fromList (fmap GLFWbVideoMode [oldMode])
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
        return $ V.fromList $ fmap GLFWbVideoMode $ filter (validDisplayMode oldMode) modes

  where validDisplayMode :: GLFW.VideoMode -> GLFW.VideoMode -> Bool
        validDisplayMode oldMode newMode =
          GLFW.videoModeRedBits oldMode == GLFW.videoModeRedBits newMode &&
          GLFW.videoModeGreenBits oldMode == GLFW.videoModeGreenBits newMode &&
          GLFW.videoModeBlueBits oldMode == GLFW.videoModeBlueBits newMode &&
          GLFW.videoModeRefreshRate oldMode >= GLFW.videoModeRefreshRate newMode &&
          GLFW.videoModeWidth newMode >= 320 &&
          GLFW.videoModeHeight newMode >= 240

-- first param is dim but it is not used anywhere for some reason
glDriverSetMode :: (Int, Int) -> Int -> Bool -> Quake Int
glDriverSetMode _ mode fullscreen = do
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
        glDriverShutdown

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

        zoom (fastRenderAPIGlobals.frVid) $ do
          vdNewWidth .= newDim^._1
          vdNewHeight .= newDim^._2

        -- let the sound and input subsystems know about the new window
        VID.newWindow (newDim^._1) (newDim^._2)

        return RenderAPIConstants.rsErrOk

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

glDriverShutdown :: Quake ()
glDriverShutdown =
    (use $ glfwbGlobals.glfwbWindow) >>= \w ->
      when (isJust w) $ do
        io (GLFW.destroyWindow (fromJust w))
        glfwbGlobals.glfwbWindow .= Nothing

glDriverInit :: Int -> Int -> Quake Bool
glDriverInit xpos ypos = do
    -- do nothing
    glfwbGlobals.glfwbWindowXPos .= xpos
    glfwbGlobals.glfwbWindowYPos .= ypos
    return True

glDriverBeginFrame :: Float -> Quake ()
glDriverBeginFrame _ = return () -- do nothing

glDriverEndFrame :: Quake ()
glDriverEndFrame = do
    GL.glFlush
    Just window <- use $ glfwbGlobals.glfwbWindow
    io $ GLFW.swapBuffers window

glDriverAppActivate :: Bool -> Quake ()
glDriverAppActivate _ = return () -- do nothing

glDriverEnableLogging :: Bool -> Quake ()
glDriverEnableLogging _ = return () -- do nothing

glDriverLogNewFrame :: Quake ()
glDriverLogNewFrame = return () -- do nothing

glDriverUpdateScreen :: XCommandT -> Quake ()
glDriverUpdateScreen callback = callback
