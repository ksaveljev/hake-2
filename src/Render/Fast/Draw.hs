{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix)
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import qualified Render.Fast.Image as Image
import qualified Render.RenderAPIConstants as RenderAPIConstants

initLocal :: Quake ()
initLocal = do
    -- load console characters (don't bilerp characters)
    Just (ImageReference imageIdx) <- Image.glFindImage "pics/conchars.pcx" RenderAPIConstants.itPic
    Just image <- preuse $ fastRenderAPIGlobals.frGLTextures.ix imageIdx
    Image.glBind (image^.iTexNum)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_NEAREST)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_NEAREST)
