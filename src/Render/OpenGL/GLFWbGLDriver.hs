{-# LANGUAGE FlexibleContexts #-}
module Render.OpenGL.GLFWbGLDriver
  ( glfwbGLDriver
  ) where

import           Client.VidDefT
import qualified Client.VIDShared as VID
import qualified Constants
import qualified QCommon.Com as Com
import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (.=), (^.), _1, _2)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW

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

glDriverInit :: Int -> Int -> Quake Bool
glDriverInit xpos ypos =
  do -- do nothing
     glfwbGlobals.glfwbWindowXPos .= xpos
     glfwbGlobals.glfwbWindowYPos .= ypos
     return True

glDriverSetMode :: (Int, Int) -> Int -> Bool -> Quake Int
glDriverSetMode _ mode fullScreen =
  do VID.printf Constants.printAll "Initializing OpenGL display\n"
     VID.printf Constants.printAll (B.concat ["...setting mode ", encode mode, ":"])
     checkOldMode =<< use (glfwbGlobals.glfwbOldDisplayMode)
     VID.getModeInfo mode >>= applyMode fullScreen

checkOldMode :: Maybe GLFW.VideoMode -> Quake ()
checkOldMode (Just _) = return ()
checkOldMode Nothing =
  do monitor <- request (io GLFW.getPrimaryMonitor)
     maybe monitorError setVideoMode monitor

monitorError :: Quake ()
monitorError = Com.fatalError "GLFWbGLDriver.glDriverSetMode monitor is Nothing"

setVideoMode :: GLFW.Monitor -> Quake ()
setVideoMode monitor =
  do videoMode <- request (io (GLFW.getVideoMode monitor))
     glfwbGlobals.glfwbOldDisplayMode .= videoMode

applyMode :: Bool -> Maybe (Int, Int) -> Quake Int
applyMode _ Nothing =
  do VID.printf Constants.printAll " invalid mode\n"
     return Constants.rsErrInvalidMode
applyMode fullScreen (Just newDim) =
  do VID.printf Constants.printAll (encode newDim)
     glDriverShutdown
     monitor <- request (io GLFW.getPrimaryMonitor)
     maybe (monitorError >> return 0) proceedApplyMode monitor
  where proceedApplyMode monitor =
          do createWindow monitor newDim fullScreen
             printFullScreenInfo monitor fullScreen
             fastRenderAPIGlobals.frVid.vdNewWidth .= newDim^._1
             fastRenderAPIGlobals.frVid.vdNewHeight .= newDim^._2
             VID.newWindow (newDim^._1) (newDim^._2)
             return Constants.rsErrOk

createWindow :: GLFW.Monitor -> (Int, Int) -> Bool -> Quake ()
createWindow monitor newDim fullScreen
  | fullScreen =
      do window <- request (io (GLFW.createWindow (newDim^._1) (newDim^._2) "Hake2 (GLFWb)" (Just monitor) Nothing))
         maybe createWindowError setWindow window
  | otherwise =
      do window <- request (io (GLFW.createWindow (newDim^._1) (newDim^._2) "Hake2 (GLFWb)" Nothing Nothing))
         maybe createWindowError setWindow window
  where createWindowError = Com.fatalError "GLFWbGLDriver createWindow returned Nothing"
        setWindow window =
          do glfwbGlobals.glfwbWindow .= Just window
             request (io (GLFW.makeContextCurrent (Just window)))

printFullScreenInfo :: GLFW.Monitor -> Bool -> Quake ()
printFullScreenInfo _ False = return ()
printFullScreenInfo monitor True =
  do videoMode <- request (io (GLFW.getVideoMode monitor))
     maybe videoModeError printVideoModeInfo videoMode
  where videoModeError = Com.fatalError "GLFWbGLDriver.GLFW.getVideoMode returned Nothing"
        printVideoModeInfo videoMode =
          VID.printf Constants.printAll (B.concat ["...setting fullscreen ", getModeString videoMode, "\n"])

getModeString :: GLFW.VideoMode -> B.ByteString
getModeString vm =
  B.concat [ encode (GLFW.videoModeWidth vm) , "x"
           , encode (GLFW.videoModeHeight vm) , "x"
           , encode (GLFW.videoModeRedBits vm), "@" -- TODO: ensure this is what we think it is
           , encode (GLFW.videoModeRefreshRate vm), "Hz"
           ]

glDriverShutdown :: Quake ()
glDriverShutdown = driverShutdown =<< use (glfwbGlobals.glfwbWindow)

driverShutdown :: Maybe GLFW.Window -> Quake ()
driverShutdown Nothing = return ()
driverShutdown (Just window) =
  do request (io (GLFW.destroyWindow window))
     glfwbGlobals.glfwbWindow .= Nothing

glDriverBeginFrame :: Float -> Quake ()
glDriverBeginFrame _ = return () -- do nothing

glDriverEndFrame :: Quake ()
glDriverEndFrame = error "GLFWbGLDriver.glDriverEndFrame" -- TODO

glDriverAppActivate :: Bool -> Quake ()
glDriverAppActivate _ = return () -- do nothing

glDriverEnableLogging :: Bool -> Quake ()
glDriverEnableLogging _ = return () -- do nothing

glDriverLogNewFrame :: Quake ()
glDriverLogNewFrame = return () -- do nothing

glDriverGetModeList :: Quake (V.Vector VideoMode)
glDriverGetModeList =
  do oldMode <- use (glfwbGlobals.glfwbOldDisplayMode)
     monitor <- request (io GLFW.getPrimaryMonitor)
     proceedGetModeList oldMode monitor
  where proceedGetModeList Nothing _ =
          do Com.fatalError "GLFWbGLDriver.glDriverGetModeList oldMode is Nothing"
             return V.empty
        proceedGetModeList _ Nothing =
          do Com.fatalError "GLFWbGLDriver.glDriverGetModeList monitor is Nothing"
             return V.empty
        proceedGetModeList (Just oldMode) (Just monitor) =
          do vm <- request (io (GLFW.getVideoModes monitor))
             constructVideoModes oldMode vm

constructVideoModes :: GLFW.VideoMode -> Maybe [GLFW.VideoMode] -> Quake (V.Vector VideoMode)
constructVideoModes oldMode Nothing =
  return (V.fromList (fmap GLFWbVideoMode [oldMode]))
constructVideoModes oldMode (Just modes) =
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
  return (V.fromList (fmap GLFWbVideoMode (filter (validDisplayMode oldMode) modes)))

validDisplayMode :: GLFW.VideoMode -> GLFW.VideoMode -> Bool
validDisplayMode oldMode newMode =
  GLFW.videoModeRedBits oldMode == GLFW.videoModeRedBits newMode &&
  GLFW.videoModeGreenBits oldMode == GLFW.videoModeGreenBits newMode &&
  GLFW.videoModeBlueBits oldMode == GLFW.videoModeBlueBits newMode &&
  GLFW.videoModeRefreshRate oldMode >= GLFW.videoModeRefreshRate newMode &&
  GLFW.videoModeWidth newMode >= 320 &&
  GLFW.videoModeHeight newMode >= 240

glDriverUpdateScreen :: XCommandT -> Quake ()
glDriverUpdateScreen = runXCommandT

glDriverSetSwapInterval :: Int -> Quake ()
glDriverSetSwapInterval v = request (io (GLFW.swapInterval v))
