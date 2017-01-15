module Render.Fast.Draw
  ( drawChar
  , drawPic
  , fadeScreen
  , fill
  , findPic
  , getPicSize
  , initLocal
  , stretchPic
  , stretchRaw
  ) where

import {-# SOURCE #-} qualified Client.VID as VID
import           Client.VidDefT
import qualified Constants
import qualified QCommon.Com as Com
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image as Image
import           Render.GLConfigT
import           Render.ImageT
import           Types

import           Control.Lens (use, (.=), (^.))
import           Control.Monad (when)
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as UV
import           Data.Word (Word8)
import qualified Graphics.GL as GL

initLocal :: Quake ()
initLocal =
  do imageRef <- Image.glFindImage "pics/conchars.pcx" Constants.itPic
     maybe imageRefError bindImageRef imageRef
  where imageRefError = error "Draw.initLocal imageRef is Nothing"
        bindImageRef imageRef =
          do fastRenderAPIGlobals.frDrawChars .= Just imageRef
             image <- readRef imageRef
             Image.glBind (image^.iTexNum)
             request setGLTexParams
        setGLTexParams =
          do GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_NEAREST)
             GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_NEAREST)

findPic :: B.ByteString -> Quake (Maybe (Ref' ImageT))
findPic name
  | BC.take 1 name == "/" || BC.take 1 name == "\\" = Image.glFindImage (B.drop 1 name) Constants.itPic
  | otherwise = Image.glFindImage name Constants.itPic

getPicSize :: B.ByteString -> Quake (Maybe (Int, Int))
getPicSize pic = picSize =<< findPic pic
  where picSize Nothing = return Nothing
        picSize (Just imageRef) =
          do image <- readRef imageRef
             return (Just (image^.iWidth, image^.iHeight))

drawPic :: Int -> Int -> B.ByteString -> Quake ()
drawPic = error "Draw.drawPic" -- TODO

stretchPic :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchPic x y w h pic =
  do imageRef <- findPic pic
     maybe imageRefError proceedStretchPic imageRef
  where imageRefError =
          VID.printf Constants.printAll (B.concat ["Can't find pic: ", pic, "\n"])
        proceedStretchPic imageRef =
          do image <- readRef imageRef
             scrapDirty <- use (fastRenderAPIGlobals.frScrapDirty)
             glConfig <- use (fastRenderAPIGlobals.frGLConfig)
             when scrapDirty Image.scrapUpload
             Image.glBind (image^.iTexNum)
             request (io (doStretchPic x y h w image glConfig))

doStretchPic :: Int -> Int -> Int -> Int -> ImageT -> GLConfigT -> IO ()
doStretchPic x y h w image glConfig =
  do when alphaTest $
       GL.glDisable GL.GL_ALPHA_TEST
     GL.glBegin GL.GL_QUADS
     GL.glTexCoord2f isl itl
     GL.glVertex2f x' y'
     GL.glTexCoord2f ish itl
     GL.glVertex2f (x' + w') y'
     GL.glTexCoord2f ish ith
     GL.glVertex2f (x' + w') (y' + h')
     GL.glTexCoord2f isl ith
     GL.glVertex2f x' (y' + h')
     GL.glEnd
     when alphaTest $
       GL.glEnable GL.GL_ALPHA_TEST
  where isl = realToFrac (image^.iSL)
        itl = realToFrac (image^.iTL)
        ith = realToFrac (image^.iTH)
        ish = realToFrac (image^.iSH)
        x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral w
        h' = fromIntegral h
        alphaTest = (((glConfig^.glcRenderer) == Constants.glRendererMCD) || ((glConfig^.glcRenderer) .&. Constants.glRendererRendition /= 0)) && not (image^.iHasAlpha)

drawChar :: Int -> Int -> Int -> Quake ()
drawChar x y num
  | n .&. 127 == 32 || y <= 8 = return ()
  | otherwise =
      do imageRef <- use (fastRenderAPIGlobals.frDrawChars)
         maybe imageRefError bindAndDraw imageRef
  where n = num .&. 255
        imageRefError = error "Draw.drawChar imageRef is Nothing"
        bindAndDraw imageRef =
          do image <- readRef imageRef
             Image.glBind (image^.iTexNum)
             request glDrawChar
        glDrawChar =
          do GL.glBegin GL.GL_QUADS
             GL.glTexCoord2f fcol frow
             GL.glVertex2f x' y'
             GL.glTexCoord2f (fcol + size) frow
             GL.glVertex2f (x' + 8) y'
             GL.glTexCoord2f (fcol + size) (frow + size)
             GL.glVertex2f (x' + 8) (y' + 8)
             GL.glTexCoord2f fcol (frow + size)
             GL.glVertex2f x' (y' + 8)
             GL.glEnd
        row = n `shiftR` 4
        col = n .&. 15
        frow = fromIntegral row * 0.0625
        fcol = fromIntegral col * 0.0625
        size = 0.0625
        x' = fromIntegral x
        y' = fromIntegral y

fill :: Int -> Int -> Int -> Int -> Int -> Quake ()
fill x y w h colorIndex
  | colorIndex > 255 =
      Com.fatalError "Draw_Fill: bad color"
  | otherwise =
      do d8to24table <- use (fastRenderAPIGlobals.frd8to24table)
         request (io (doFill x y w h (d8to24table UV.! colorIndex)))

doFill :: Int -> Int -> Int -> Int -> Int -> IO ()
doFill x y w h color =
  do GL.glDisable GL.GL_TEXTURE_2D
     GL.glColor3ub (fromIntegral ((color `shiftR`  0) .&. 0xFF)) -- r
                   (fromIntegral ((color `shiftR`  8) .&. 0xFF)) -- g
                   (fromIntegral ((color `shiftR` 16) .&. 0xFF)) -- b
     GL.glBegin GL.GL_QUADS
     GL.glVertex2f x' y'
     GL.glVertex2f (x' + w') y'
     GL.glVertex2f (x' + w') (y' + h')
     GL.glVertex2f x' (y' + h')
     GL.glEnd
     GL.glColor3f 1 1 1
     GL.glEnable GL.GL_TEXTURE_2D
  where x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral w
        h' = fromIntegral h

fadeScreen :: Quake ()
fadeScreen =
  do vid <- use (fastRenderAPIGlobals.frVid)
     request (glFade (fromIntegral (vid^.vdWidth)) (fromIntegral (vid^.vdHeight)))
  where glFade w h =
          do GL.glEnable GL.GL_BLEND
             GL.glDisable GL.GL_TEXTURE_2D
             GL.glColor4f 0 0 0 0.8
             GL.glBegin GL.GL_QUADS
             GL.glVertex2f 0 0
             GL.glVertex2f w 0
             GL.glVertex2f w h
             GL.glVertex2f 0 h
             GL.glEnd
             GL.glColor4f 1 1 1 1
             GL.glEnable GL.GL_TEXTURE_2D
             GL.glDisable GL.GL_BLEND

stretchRaw :: Int -> Int -> Int -> Int -> Int -> Int -> UV.Vector Word8 -> Quake ()
stretchRaw = error "Draw.stretchRaw" -- TODO
