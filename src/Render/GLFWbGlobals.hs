{-# LANGUAGE TemplateHaskell #-}
module Render.GLFWbGlobals
  ( module Render.GLFWbGlobals
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''GLFWbGlobals

initialGLFWbGlobals :: GLFWbGlobals
initialGLFWbGlobals =
  GLFWbGlobals { _glfwbOldDisplayMode = Nothing
               , _glfwbWindow         = Nothing
               , _glfwbWindowXPos     = 0
               , _glfwbWindowYPos     = 0
               , _glfwbKBDChan        = Nothing
               }