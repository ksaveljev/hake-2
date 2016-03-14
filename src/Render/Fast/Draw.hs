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

import           Client.VidDefT
import qualified Constants
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image as Image
import           Render.ImageT
import           Types

import           Control.Lens (use, (.=), (^.))
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Graphics.GL as GL

initLocal :: Quake ()
initLocal =
  do imageRef <- Image.glFindImage "pics/conchars.pcx" Constants.itPic
     error "Pfff" -- TODO REMOVE
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

findPic :: B.ByteString -> Quake (Maybe (Ref ImageT))
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
stretchPic = error "Draw.stretchPic" -- TODO

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
fill = error "Draw.fill" -- TODO

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

stretchRaw :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchRaw = error "Draw.stretchRaw" -- TODO
