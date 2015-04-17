{-# LANGUAGE TemplateHaskell #-}
module Sys.GLFWbKBDGlobals ( module Sys.GLFWbKBDGlobals
                           ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''GLFWbKBDGlobals

initialGLFWbKBDGlobals :: GLFWbKBDGlobals
initialGLFWbKBDGlobals =
  GLFWbKBDGlobals { _glfwbKBDmx    = 0
                  , _glfwbKBDmy    = 0
                  , _glfwbKBDwinx  = 0
                  , _glfwbKBDwiny  = 0
                  }
