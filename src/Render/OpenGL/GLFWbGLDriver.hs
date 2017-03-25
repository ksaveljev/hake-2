{-# LANGUAGE FlexibleContexts #-}
module Render.OpenGL.GLFWbGLDriver
    ( glfwbGLDriver
    ) where

import           Control.Lens     (use, (.=), (^.), _1, _2)
import qualified Data.ByteString  as B
import qualified Data.Vector      as V
import qualified Graphics.UI.GLFW as GLFW

import           Types

glfwbGLDriver :: GLDriver
glfwbGLDriver = error "GLFWbGLDriver.glfwbGLDriver" -- TODO