{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix, use)
import Control.Monad (when)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import Render.OpenGL.GLDriver
import qualified Constants
import qualified Client.VID as VID
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
stretchPic _ x y w h pic = do
    maybeImage <- findPic pic

    case maybeImage of
      Nothing ->
        VID.printf Constants.printAll $ "Can't find pic: " `B.append` pic `B.append` "\n"
      Just (ImageReference imageIdx) -> do
        Just image <- preuse $ fastRenderAPIGlobals.frGLTextures.ix imageIdx
        scrapDirty <- use $ fastRenderAPIGlobals.frScrapDirty
        glConfig <- use $ fastRenderAPIGlobals.frGLConfig

        when scrapDirty Image.scrapUpload

        when ((((glConfig^.glcRenderer) == RenderAPIConstants.glRendererMCD) || ((glConfig^.glcRenderer) .&. RenderAPIConstants.glRendererRendition /= 0)) && not (image^.iHasAlpha)) $
          GL.glDisable GL.gl_ALPHA_TEST

        let isl = realToFrac $ image^.iSL
            itl = realToFrac $ image^.iTL
            ith = realToFrac $ image^.iTH
            ish = realToFrac $ image^.iSH
            x' = fromIntegral x
            y' = fromIntegral y
            w' = fromIntegral w
            h' = fromIntegral h

        Image.glBind (image^.iTexNum)
        GL.glBegin GL.gl_QUADS
        GL.glTexCoord2f isl itl
        GL.glVertex2f x' y'
        GL.glTexCoord2f ish itl
        GL.glVertex2f (x' + w') y'
        GL.glTexCoord2f ish ith
        GL.glVertex2f (x' + w') (y' + h')
        GL.glTexCoord2f isl ith
        GL.glVertex2f x' (y' + h')
        GL.glEnd

        when ((((glConfig^.glcRenderer) == RenderAPIConstants.glRendererMCD) || ((glConfig^.glcRenderer) .&. RenderAPIConstants.glRendererRendition /= 0)) && not (image^.iHasAlpha)) $
          GL.glEnable GL.gl_ALPHA_TEST

findPic :: B.ByteString -> Quake (Maybe ImageReference)
findPic name =
    if BC.take 1 name == "/" || BC.take 1 name == "\\"
      then Image.glFindImage (B.drop 1 name) RenderAPIConstants.itPic
      else Image.glFindImage name RenderAPIConstants.itPic
