{-# LANGUAGE OverloadedStrings #-}
module Render.GLFWbRenderer where

import qualified Debug.Trace as DT

import Render.Basic.BasicRenderAPI
import Render.Renderer

glfwbRenderer :: Renderer
glfwbRenderer = 
  Renderer { _rName      = "GLFWb"
           , _rRefExport = glfwbRefExportT
           , _rRenderAPI = basicRenderAPI
           }

glfwbRefExportT :: RefExportT
glfwbRefExportT = DT.trace "GLFWbRenderer.glfwbRefExportT" undefined -- TODO
