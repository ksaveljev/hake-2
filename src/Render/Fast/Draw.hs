{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix, use, (.=))
import Control.Monad (when, unless)
import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, readIORef)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import qualified Render.Fast.Image as Image
import qualified Render.RenderAPIConstants as RenderAPIConstants

initLocal :: Quake ()
initLocal = do
    -- load console characters (don't bilerp characters)
    Just imageRef <- Image.glFindImage "pics/conchars.pcx" RenderAPIConstants.itPic
    fastRenderAPIGlobals.frDrawChars .= Just imageRef

    image <- io $ readIORef imageRef
    Image.glBind (image^.iTexNum)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_NEAREST)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_NEAREST)

stretchPic :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchPic x y w h pic = do
    maybeImage <- findPic pic

    case maybeImage of
      Nothing ->
        VID.printf Constants.printAll $ "Can't find pic: " `B.append` pic `B.append` "\n"
      Just imageRef -> do
        image <- io $ readIORef imageRef
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

findPic :: B.ByteString -> Quake (Maybe (IORef ImageT))
findPic name =
    if BC.take 1 name == "/" || BC.take 1 name == "\\"
      then Image.glFindImage (B.drop 1 name) RenderAPIConstants.itPic
      else Image.glFindImage name RenderAPIConstants.itPic

{-
================
Draw_Char
Draws one 8*8 graphics character with 0 being transparent.
It can be clipped to the top of the screen to allow the console to be
smoothly scrolled off.
================
-}
drawChar :: Int -> Int -> Int -> Quake ()
drawChar x y num = do
    let n = num .&. 255

        -- it's a space   -- totally off screen
    unless (n .&. 127 == 32 || y <= -8) $ do
      let row = n `shiftR` 4
          col = n .&. 15
          frow = fromIntegral row * 0.0625
          fcol = fromIntegral col * 0.0625
          size = 0.0625
          x' = fromIntegral x
          y' = fromIntegral y

      Just imageRef <- use $ fastRenderAPIGlobals.frDrawChars
      image <- io $ readIORef imageRef

      Image.glBind (image^.iTexNum)

      GL.glBegin GL.gl_QUADS
      GL.glTexCoord2f fcol frow
      GL.glVertex2f x' y'
      GL.glTexCoord2f (fcol + size) frow
      GL.glVertex2f (x' + 8) y'
      GL.glTexCoord2f (fcol + size) (frow + size)
      GL.glVertex2f (x' + 8) (y' + 8)
      GL.glTexCoord2f fcol (frow + size)
      GL.glVertex2f x' (y' + 8)
      GL.glEnd

fill :: Int -> Int -> Int -> Int -> Int -> Quake ()
fill x y w h colorIndex = do
    when (colorIndex > 255) $
      Com.comError Constants.errFatal "Draw_Fill: bad color"

    GL.glDisable GL.gl_TEXTURE_2D

    d8to24table <- use $ fastRenderAPIGlobals.frd8to24table
    let color = d8to24table UV.! colorIndex

    GL.glColor3ub (fromIntegral $ (color `shiftR`  0) .&. 0xFF) -- r
                  (fromIntegral $ (color `shiftR`  8) .&. 0xFF) -- g
                  (fromIntegral $ (color `shiftR` 16) .&. 0xFF) -- b

    let x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral w
        h' = fromIntegral h

    GL.glBegin GL.gl_QUADS

    GL.glVertex2f x' y'
    GL.glVertex2f (x' + w') y'
    GL.glVertex2f (x' + w') (y' + h')
    GL.glVertex2f x' (y' + h')

    GL.glEnd
    GL.glColor3f 1 1 1
    GL.glEnable GL.gl_TEXTURE_2D

getPicSize :: B.ByteString -> Quake (Maybe (Int, Int))
getPicSize pic = do
    maybeImage <- findPic pic

    case maybeImage of
      Nothing -> return Nothing
      Just imageRef -> do
        image <- io $ readIORef imageRef
        return $ Just (image^.iWidth, image^.iHeight)

drawPic :: Int -> Int -> B.ByteString -> Quake ()
drawPic x y pic = do
    foundImage <- findPic pic

    case foundImage of
      Nothing ->
        VID.printf Constants.printAll ("Can't find pic: " `B.append` pic `B.append` "\n")
      Just imageRef -> do
        scrapDirty <- use $ fastRenderAPIGlobals.frScrapDirty
        image <- io $ readIORef imageRef
        glConfig <- use $ fastRenderAPIGlobals.frGLConfig

        when scrapDirty $
          Image.scrapUpload

        when ((((glConfig^.glcRenderer) == RenderAPIConstants.glRendererMCD) || ((glConfig^.glcRenderer) .&. RenderAPIConstants.glRendererRendition /= 0)) && not (image^.iHasAlpha)) $
          GL.glDisable GL.gl_ALPHA_TEST

        let isl = realToFrac $ image^.iSL
            itl = realToFrac $ image^.iTL
            ith = realToFrac $ image^.iTH
            ish = realToFrac $ image^.iSH
            x' = fromIntegral x
            y' = fromIntegral y
            w' = fromIntegral (image^.iWidth)
            h' = fromIntegral (image^.iHeight)

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
