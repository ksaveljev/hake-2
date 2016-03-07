module Render.OpenGL.GLFWbGLDriver
  ( glfwbGLDriver
  ) where

import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import           Types

import           Control.Lens (use, (.=))
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
glDriverSetMode = error "GLFWbGLDriver.glDriverSetMode" -- TODO

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
glDriverGetModeList = error "GLFWbGLDriver.glDriverGetModeList" -- TODO

glDriverUpdateScreen :: XCommandT -> Quake ()
glDriverUpdateScreen = runXCommandT

glDriverSetSwapInterval :: Int -> Quake ()
glDriverSetSwapInterval v = request (io (GLFW.swapInterval v))