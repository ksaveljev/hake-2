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

import           Control.Lens            (use, (.=), (^.))
import           Control.Monad           (when)
import           Data.Bits               (shiftR, (.&.))
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Unsafe  as BU
import           Data.Monoid             ((<>))
import qualified Data.Vector.Unboxed     as UV
import           Data.Word               (Word8)
import qualified Graphics.GL             as GL

import {-# SOURCE #-} qualified Client.VID as VID
import           Client.VidDefT
import qualified Constants
import qualified QCommon.Com             as Com
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image       as Image
import           Render.GLConfigT
import           Render.ImageT
import           Types

initLocal :: Quake ()
initLocal = do
    imageRef <- Image.glFindImage "pics/conchars.pcx" Constants.itPic
    maybe imageRefError bindImageRef imageRef
  where
    imageRefError = error "Draw.initLocal imageRef is Nothing"
    bindImageRef imageRef = do
        fastRenderAPIGlobals.frDrawChars .= Just imageRef
        image <- readRef imageRef
        Image.glBind (image^.iTexNum)
        io setGLTexParams
    setGLTexParams = do
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_NEAREST)
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_NEAREST)

findPic :: B.ByteString -> Quake (Maybe (Ref ImageT))
findPic name
    | BC.take 1 name == "/" || BC.take 1 name == "\\" = Image.glFindImage (B.drop 1 name) Constants.itPic
    | otherwise = Image.glFindImage name Constants.itPic

getPicSize :: B.ByteString -> Quake (Maybe (Int, Int))
getPicSize pic =
    picSize =<< findPic pic
  where
    picSize Nothing = return Nothing
    picSize (Just imageRef) = do
        image <- readRef imageRef
        return (Just (image^.iWidth, image^.iHeight))

drawPic :: Int -> Int -> B.ByteString -> Quake ()
drawPic x y pic = do
    foundImage <- findPic pic
    maybe imageNotFound (doDrawPic x y) foundImage
  where
    imageNotFound =
        VID.printf Constants.printAll (B.concat ["Can't find pic: ", pic, "\n"])

doDrawPic :: Int -> Int -> Ref ImageT -> Quake ()
doDrawPic x y imageRef = do
    image <- readRef imageRef
    scrapDirty <- use (fastRenderAPIGlobals.frScrapDirty)
    glConfig <- use (fastRenderAPIGlobals.frGLConfig)
    when scrapDirty $
        Image.scrapUpload
    when ((((glConfig^.glcRenderer) == Constants.glRendererMCD) || ((glConfig^.glcRenderer) .&. Constants.glRendererRendition /= 0)) && not (image^.iHasAlpha)) $
        io (GL.glDisable GL.GL_ALPHA_TEST)
    let isl = realToFrac (image^.iSL)
        itl = realToFrac (image^.iTL)
        ith = realToFrac (image^.iTH)
        ish = realToFrac (image^.iSH)
        x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral (image^.iWidth)
        h' = fromIntegral (image^.iHeight)
    Image.glBind (image^.iTexNum)
    io $ do
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
    when ((((glConfig^.glcRenderer) == Constants.glRendererMCD) || ((glConfig^.glcRenderer) .&. Constants.glRendererRendition /= 0)) && not (image^.iHasAlpha)) $
        io (GL.glEnable GL.GL_ALPHA_TEST)

stretchPic :: Int -> Int -> Int -> Int -> B.ByteString -> Quake ()
stretchPic x y w h pic = do
    imageRef <- findPic pic
    maybe imageRefError proceedStretchPic imageRef
  where
    imageRefError =
        VID.printf Constants.printAll (B.concat ["Can't find pic: ", pic, "\n"])
    proceedStretchPic imageRef = do
        image <- readRef imageRef
        scrapDirty <- use (fastRenderAPIGlobals.frScrapDirty)
        glConfig <- use (fastRenderAPIGlobals.frGLConfig)
        when scrapDirty Image.scrapUpload
        Image.glBind (image^.iTexNum)
        io (doStretchPic x y h w image glConfig)

doStretchPic :: Int -> Int -> Int -> Int -> ImageT -> GLConfigT -> IO ()
doStretchPic x y h w image glConfig = do
    when alphaTest $
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
  where
    isl = realToFrac (image^.iSL)
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
    | otherwise = do
        imageRef <- use (fastRenderAPIGlobals.frDrawChars)
        maybe imageRefError bindAndDraw imageRef
  where
    n = num .&. 255
    imageRefError = error "Draw.drawChar imageRef is Nothing"
    bindAndDraw imageRef = do
        image <- readRef imageRef
        Image.glBind (image^.iTexNum)
        io glDrawChar
    glDrawChar = do
        GL.glBegin GL.GL_QUADS
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
    | otherwise = do
        d8to24table <- use (fastRenderAPIGlobals.frd8to24table)
        io (doFill x y w h (d8to24table UV.! colorIndex))

doFill :: Int -> Int -> Int -> Int -> Int -> IO ()
doFill x y w h color = do
    GL.glDisable GL.GL_TEXTURE_2D
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
  where
    x' = fromIntegral x
    y' = fromIntegral y
    w' = fromIntegral w
    h' = fromIntegral h

fadeScreen :: Quake ()
fadeScreen = do
    vid <- use (fastRenderAPIGlobals.frVid)
    glFade (fromIntegral (vid^.vdWidth)) (fromIntegral (vid^.vdHeight))
  where
    glFade w h = io $ do
        GL.glEnable GL.GL_BLEND
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
stretchRaw x y w h cols rows buffer = do
    Image.glBind 0
    doStretchRaw =<< use (fastRenderAPIGlobals.frColorTableEXT)
    io $ do
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
    glConfig <- use (fastRenderAPIGlobals.frGLConfig)
    when ((((glConfig^.glcRenderer) == Constants.glRendererMCD) || ((glConfig^.glcRenderer) .&. Constants.glRendererRendition /= 0))) $
        io (GL.glDisable GL.GL_ALPHA_TEST)
    let x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral w
        h' = fromIntegral h
        t' = realToFrac t
    io $ do
        GL.glBegin GL.GL_QUADS
        GL.glTexCoord2f 0 0
        GL.glVertex2f x' y'
        GL.glTexCoord2f 1 0
        GL.glVertex2f (x' + w') y'
        GL.glTexCoord2f 1 t'
        GL.glVertex2f (x' + w') (y' + h')
        GL.glTexCoord2f 0 t'
        GL.glVertex2f x' (y' + h')
        GL.glEnd
    when ((((glConfig^.glcRenderer) == Constants.glRendererMCD) || ((glConfig^.glcRenderer) .&. Constants.glRendererRendition /= 0))) $
        io (GL.glEnable GL.GL_ALPHA_TEST)
  where
    (hScale, trows)
        | rows <= 256 = (1, rows)
        | otherwise   = ((fromIntegral rows) / 256.0, 256)
    t = (fromIntegral rows) * hScale / 256 :: Float
    doStretchRaw :: Bool -> Quake ()
    doStretchRaw colorTableExt
        | colorTableExt =
            error "Draw.stretchRaw when colorTableExt = True [not implemented]"
        | otherwise = do
            rawPalette <- use (fastRenderAPIGlobals.frRawPalette)
            glTexSolidFormat <- use (fastRenderAPIGlobals.frGLTexSolidFormat)
            let image32 = BL.toStrict (BB.toLazyByteString (buildImage32 rawPalette mempty 0 trows))
                image32' = image32 `B.append` (B.replicate ((256 * 256 * 8) - (B.length image32)) 0)
            io $ BU.unsafeUseAsCString image32' $ \ptr ->
                GL.glTexImage2D GL.GL_TEXTURE_2D
                                0
                                (fromIntegral glTexSolidFormat)
                                256
                                256
                                0
                                GL.GL_RGBA
                                GL.GL_UNSIGNED_BYTE
                                ptr
    buildImage32 :: UV.Vector Int -> BB.Builder -> Int -> Int -> BB.Builder
    buildImage32 rawPalette builder idx maxIdx
        | idx >= maxIdx = builder
        | otherwise =
            let row = truncate (fromIntegral idx * hScale)
            in if row > rows
               then builder
               else let sourceIndex = cols * row
                        fracStep = cols * 0x10000 `div` 256
                        frac = fracStep `shiftR` 1
                        builder' = buildRow32 rawPalette builder sourceIndex frac fracStep 0 256
                    in buildImage32 rawPalette builder' (idx + 1) maxIdx
    buildRow32 :: UV.Vector Int -> BB.Builder -> Int -> Int -> Int -> Int -> Int -> BB.Builder
    buildRow32 rawPalette builder sourceIndex frac fracStep idx maxIdx
        | idx >= maxIdx = builder
        | otherwise =
            let v = rawPalette UV.! fromIntegral (buffer UV.! (sourceIndex + (frac `shiftR` 16)))
            in buildRow32 rawPalette (builder <> BB.int32LE (fromIntegral v)) sourceIndex (frac + fracStep) fracStep (idx + 1) maxIdx