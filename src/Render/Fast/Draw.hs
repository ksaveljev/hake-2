{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Draw where

import Control.Lens (preuse, (^.), ix, use, (.=))
import Control.Monad (when, unless)
import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, readIORef)
import Data.Monoid (mempty, (<>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
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

stretchRaw :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchRaw x y w h cols rows buffer = do
    Image.glBind 0
    let (hScale, trows) = if rows <= 256
                            then (1, rows)
                            else ((fromIntegral rows) / 256.0, 256)
        t = (fromIntegral rows) * hScale / 256 :: Float

    colorTableExt <- use $ fastRenderAPIGlobals.frColorTableEXT

    if colorTableExt
      then do
        io (putStrLn "Draw.stretchRaw") >> undefined -- TODO

      else do
        rawPalette <- use $ fastRenderAPIGlobals.frRawPalette
        let image32 = BL.toStrict (BB.toLazyByteString (buildImage32 rawPalette hScale mempty 0 trows))

        glTexSolidFormat <- use $ fastRenderAPIGlobals.frGLTexSolidFormat
        io $ BU.unsafeUseAsCString image32 $ \ptr ->
          GL.glTexImage2D GL.gl_TEXTURE_2D
                          0
                          (fromIntegral glTexSolidFormat)
                          256
                          256
                          0
                          GL.gl_RGBA
                          GL.gl_UNSIGNED_BYTE
                          ptr

    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_LINEAR)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_LINEAR)

    glConfig <- use $ fastRenderAPIGlobals.frGLConfig

    when ((((glConfig^.glcRenderer) == RenderAPIConstants.glRendererMCD) || ((glConfig^.glcRenderer) .&. RenderAPIConstants.glRendererRendition /= 0))) $
      GL.glDisable GL.gl_ALPHA_TEST

    let x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral w
        h' = fromIntegral h
        t' = realToFrac t

    GL.glBegin GL.gl_QUADS
    GL.glTexCoord2f 0 0
    GL.glVertex2f x' y'
    GL.glTexCoord2f 1 0
    GL.glVertex2f (x' + w') y'
    GL.glTexCoord2f 1 t'
    GL.glVertex2f (x' + w') (y' + h')
    GL.glTexCoord2f 0 t'
    GL.glVertex2f x' (y' + h')
    GL.glEnd

    when ((((glConfig^.glcRenderer) == RenderAPIConstants.glRendererMCD) || ((glConfig^.glcRenderer) .&. RenderAPIConstants.glRendererRendition /= 0))) $
      GL.glEnable GL.gl_ALPHA_TEST

  where buildImage32 :: UV.Vector Int -> Float -> BB.Builder -> Int -> Int -> BB.Builder
        buildImage32 rawPalette hScale builder idx maxIdx
          | idx >= maxIdx = builder
          | otherwise =
              let row = truncate (fromIntegral idx * hScale)
              in if row > rows
                   then builder
                   else let sourceIndex = cols * row
                            fracStep = cols * 0x10000 `div` 256
                            frac = fracStep `shiftR` 1
                            builder' = buildRow32 rawPalette builder sourceIndex frac fracStep 0 256
                        in buildImage32 rawPalette hScale builder' (idx + 1) maxIdx

        buildRow32 :: UV.Vector Int -> BB.Builder -> Int -> Int -> Int -> Int -> Int -> BB.Builder
        buildRow32 rawPalette builder sourceIndex frac fracStep idx maxIdx
          | idx >= maxIdx = builder
          | otherwise =
              let v = rawPalette UV.! fromIntegral (buffer `B.index` (sourceIndex + (frac `shiftR` 16)))
              in buildRow32 rawPalette (builder <> BB.int32LE (fromIntegral v)) sourceIndex (frac + fracStep) fracStep (idx + 1) maxIdx

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

fadeScreen :: Quake ()
fadeScreen = do
    vid <- use $ fastRenderAPIGlobals.frVid

    let w = fromIntegral (vid^.vdWidth)
        h = fromIntegral (vid^.vdHeight)

    GL.glEnable GL.gl_BLEND
    GL.glDisable GL.gl_TEXTURE_2D
    GL.glColor4f 0 0 0 0.8
    GL.glBegin GL.gl_QUADS

    GL.glVertex2f 0 0
    GL.glVertex2f w 0
    GL.glVertex2f w h
    GL.glVertex2f 0 h

    GL.glEnd
    GL.glColor4f 1 1 1 1
    GL.glEnable GL.gl_TEXTURE_2D
    GL.glDisable GL.gl_BLEND
