{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import Render.OpenGL.GLDriver
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

stretchPic :: GLDriver -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchPic glDriver x y w h pic = do
    io (putStrLn "FastRenderAPI.fastDrawStretchPic") >> undefined -- TODO

findPic :: B.ByteString -> Quake (Maybe ImageReference)
findPic name =
    if BC.take 1 name == "/" || BC.take 1 name == "\\"
      then Image.glFindImage (B.drop 1 name) RenderAPIConstants.itPic
      else Image.glFindImage name RenderAPIConstants.itPic
