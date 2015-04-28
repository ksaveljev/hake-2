module Render.VideoMode ( VideoMode(..)
                        , module Render.VideoMode
                        ) where

import qualified Graphics.UI.GLFW as GLFW

import Internal

getVideoModeWidth :: VideoMode -> Int
getVideoModeWidth (GLFWbVideoMode mode) = GLFW.videoModeWidth mode

getVideoModeHeight :: VideoMode -> Int
getVideoModeHeight (GLFWbVideoMode mode) = GLFW.videoModeHeight mode

getVideoModeRefreshRate :: VideoMode -> Int
getVideoModeRefreshRate (GLFWbVideoMode mode) = GLFW.videoModeRefreshRate mode
