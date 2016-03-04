module Render.GLFWbRenderer
  ( glfwbRenderer
  ) where

import Render.Basic.BasicRenderAPI (basicRenderAPI)
import Types

glfwbRefExport :: RenderAPI -> RefExportT
glfwbRefExport = glfwbRefExportT glfwbKBD

glfwbRenderer :: Renderer
glfwbRenderer = 
  Renderer { _rName      = "GLFWb"
           , _rRefExport = glfwbRefExportT glfwbKBD basicRenderAPI
           }

glfwbRefExportT :: KBD -> RenderAPI -> RefExportT
glfwbRefExportT = error "GLFWbRenderer.glfwbRefExportT"

glfwbKBD :: KBD
glfwbKBD = error "Render.glfwbKBD"