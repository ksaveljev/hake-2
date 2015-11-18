{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
module Render.Fast.Image where

import Control.Lens ((^.), (.=), (+=), use, preuse, ix, _1, _2, zoom, (%=))
import Control.Monad (when, void, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (toUpper)
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, modifyIORef', writeIORef)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.Monoid (mappend, mempty, mconcat)
import Data.Word (Word8)
import Foreign.Marshal.Utils (with)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import CVarVariables
import QCommon.MiptexT
import QCommon.QFiles.PcxT
import QCommon.QFiles.TgaT
import QCommon.XCommandT
import Render.GLModeT
import Render.GLTModeT
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.RenderAPIConstants as RenderAPIConstants

glSolidFormat :: Int
glSolidFormat = 3

glAlphaFormat :: Int
glAlphaFormat = 4

numGLModes :: Int
numGLModes = V.length modes

numGLAlphaModes :: Int
numGLAlphaModes = V.length glAlphaModes

numGLSolidModes :: Int
numGLSolidModes = V.length glSolidModes

-- must be a power of2
floodFillFifoSize :: Int
floodFillFifoSize = 0x1000

floodFillFifoMask :: Int
floodFillFifoMask = floodFillFifoSize - 1

modes :: V.Vector GLModeT
modes =
    V.fromList [ GLModeT "GL_NEAREST"                (fromIntegral GL.gl_NEAREST               ) (fromIntegral GL.gl_NEAREST)
               , GLModeT "GL_LINEAR"                 (fromIntegral GL.gl_LINEAR                ) (fromIntegral GL.gl_LINEAR )
               , GLModeT "GL_NEAREST_MIPMAP_NEAREST" (fromIntegral GL.gl_NEAREST_MIPMAP_NEAREST) (fromIntegral GL.gl_NEAREST)
               , GLModeT "GL_LINEAR_MIPMAP_NEAREST"  (fromIntegral GL.gl_LINEAR_MIPMAP_NEAREST ) (fromIntegral GL.gl_LINEAR )
               , GLModeT "GL_NEAREST_MIPMAP_LINEAR"  (fromIntegral GL.gl_NEAREST_MIPMAP_LINEAR ) (fromIntegral GL.gl_NEAREST)
               , GLModeT "GL_LINEAR_MIPMAP_LINEAR"   (fromIntegral GL.gl_LINEAR_MIPMAP_LINEAR  ) (fromIntegral GL.gl_LINEAR )
               ]

glAlphaModes :: V.Vector GLTModeT
glAlphaModes =
    V.fromList [ GLTModeT "default"    4
               , GLTModeT "GL_RGBA"    (fromIntegral GL.gl_RGBA   )
               , GLTModeT "GL_RGBA8"   (fromIntegral GL.gl_RGBA8  )
               , GLTModeT "GL_RGB5_A1" (fromIntegral GL.gl_RGB5_A1)
               , GLTModeT "GL_RGBA4"   (fromIntegral GL.gl_RGBA4  )
               , GLTModeT "GL_RGBA2"   (fromIntegral GL.gl_RGBA2  )
               ]

glSolidModes :: V.Vector GLTModeT
glSolidModes =
    V.fromList [ GLTModeT "default"     3
               , GLTModeT "GL_RGB"      (fromIntegral GL.gl_RGB     )
               , GLTModeT "GL_RGB8"     (fromIntegral GL.gl_RGB8    )
               , GLTModeT "GL_RGB5"     (fromIntegral GL.gl_RGB5    )
               , GLTModeT "GL_RGB4"     (fromIntegral GL.gl_RGB4    )
               , GLTModeT "GL_R3_G3_B2" (fromIntegral GL.gl_R3_G3_B2)
               ]

getPalette :: Quake ()
getPalette = do
    (_, result, _) <- loadPCX "pics/colormap.pcx" True False

    case result of
      Nothing -> Com.comError Constants.errFatal "Couldn't load pics/colormap.pcx"
      Just palette -> do
        when (B.length palette /= 768) $
          Com.comError Constants.errFatal "Couldn't load pics/colormap.pcx"

        let table = UV.fromList $! map (constructPalette palette) [0..255]
            lastElem = (constructPalette palette 255) .&. 0x00FFFFFF -- 255 is transparent
            d8to24table = table UV.// [(255, lastElem)]

        fastRenderAPIGlobals.frd8to24table .= d8to24table

        particleTGlobals.pColorTable .= UV.map ((.&. 0x00FFFFFF) . fromIntegral) d8to24table

  where constructPalette :: B.ByteString -> Int -> Int
        constructPalette bs i =
          let j = i * 3
              r :: Int = fromIntegral $ B.index bs j
              g :: Int = fromIntegral $ B.index bs (j + 1)
              b :: Int = fromIntegral $ B.index bs (j + 2)
          in (255 `shiftL` 24) .|. (b `shiftL` 16) .|. (g `shiftL` 8) .|. r

loadPCX :: B.ByteString -> Bool -> Bool -> Quake (Maybe B.ByteString, Maybe B.ByteString, Maybe (Int, Int))
loadPCX fileName returnPalette returnDimensions = do
    -- load the file
    file <- FS.loadFile fileName

    case file of
      Nothing -> do
        VID.printf Constants.printDeveloper ("Bad pcx file " `B.append` fileName `B.append` "\n")
        return (Nothing, Nothing, Nothing)
      Just raw -> do
        -- parse the PCX file
        let pcx = newPcxT (BL.fromStrict raw)

        if (pcx^.pcxManufacturer) /= 0x0A ||
           (pcx^.pcxVersion) /= 5 ||
           (pcx^.pcxEncoding) /= 1 ||
           (pcx^.pcxBitsPerPixel) /= 8 ||
           (pcx^.pcxXMax) >= 640 ||
           (pcx^.pcxYMax) >= 480
          then do
            VID.printf Constants.printAll ("Bad pcx file " `B.append` fileName `B.append` "\n")
            return (Nothing, Nothing, Nothing)
          else do
            let width = fromIntegral $ (pcx^.pcxXMax) - (pcx^.pcxXMin) + 1
                height = fromIntegral $ (pcx^.pcxYMax) - (pcx^.pcxYMin) + 1
                palette = if returnPalette
                            then Just (B.drop (B.length raw - 768) raw)
                            else Nothing
                dimensions = if returnDimensions
                               then Just (width, height)
                               else Nothing

            -- decode pcx
            let pix = decodePCX (BL.toStrict $ pcx^.pcxData) 0 0 0 width height mempty

            return (Just pix, palette, dimensions)

  where decodePCX :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        decodePCX raw idx x y maxX maxY acc
          | y >= maxY = BL.toStrict $ BB.toLazyByteString acc
          | x >= maxX = decodePCX raw idx 0 (y + 1) maxX maxY acc
          | otherwise =
              let dataByte = B.index raw idx
              in if dataByte .&. 0xC0 == 0xC0
                   then let runLength = fromIntegral $ dataByte .&. 0x3F
                            byte = B.index raw (idx + 1)
                            -- write runLength pixel
                        -- in decodePCX raw (idx + 2) (x + runLength) y maxX maxY (acc `mappend` mconcat (replicate runLength (BB.word8 byte)))
                        in decodePCX raw (idx + 2) (x + runLength) y maxX maxY (buildAcc acc (BB.word8 byte) runLength) -- (acc `mappend` mconcat (replicate runLength (BB.word8 byte)))
                   else -- write one pixel
                     decodePCX raw (idx + 1) (x + 1) y maxX maxY (acc `mappend` BB.word8 dataByte)

        buildAcc :: BB.Builder -> BB.Builder -> Int -> BB.Builder
        buildAcc acc byte idx
          | idx <= 0 = acc
          | otherwise = buildAcc (acc `mappend` byte) byte (idx - 1)

glImageListF :: XCommandT
glImageListF =
  XCommandT "Image.glImageListF" (do
    io (putStrLn "Image.glImageListF") >> undefined -- TODO
  )

glInitImages :: Quake ()
glInitImages = do
    fastRenderAPIGlobals.frRegistrationSequence .= 1

    -- init intensity conversions
    void $ CVar.get "intensity" "2" 0

    liftM (^.cvValue) intensityCVar >>= \v ->
      when (v <= 1) $
        void $ CVar.set "intensity" "1"

    liftM (^.cvValue) intensityCVar >>= \v ->
      fastRenderAPIGlobals.frGLState.glsInverseIntensity .= 1 / v

    getPalette

    use (fastRenderAPIGlobals.frColorTableEXT) >>= \v ->
      when v $ do
        FS.loadFile "pics/16to8.dat" >>= \buf -> do
          fastRenderAPIGlobals.frGLState.glsD16To8Table .= buf
          when (isNothing buf) $
            Com.comError Constants.errFatal "Couldn't load pics/16to8.pcx"

    renderer <- use $ fastRenderAPIGlobals.frGLConfig.glcRenderer
    g <- if renderer .&. (RenderAPIConstants.glRendererVoodoo .|. RenderAPIConstants.glRendererVoodoo2) /= 0
           then return 1
           else liftM (^.cvValue) vidGammaCVar

    fastRenderAPIGlobals.frGammaTable .= (B.unfoldr (if g == 1 then simpleGamma else complexGamma g) 0)
    liftM (^.cvValue) intensityCVar >>= \v ->
      fastRenderAPIGlobals.frIntensityTable .= (B.unfoldr (genIntensityTable v) 0)

  where simpleGamma :: Int -> Maybe (Word8, Int)
        simpleGamma idx
          | idx >= 256 = Nothing
          | otherwise = Just (fromIntegral idx, idx + 1)

        complexGamma :: Float -> Int -> Maybe (Word8, Int)
        complexGamma g idx
          | idx >= 256 = Nothing
          | otherwise =
              let inf :: Int = truncate (255 * (((fromIntegral idx + 0.5) / 255.5) ** g) + 0.5)
                  inf' = if | inf < 0 -> 0
                            | inf > 255 -> 255
                            | otherwise -> inf
              in Just (fromIntegral inf', idx + 1)

        genIntensityTable :: Float -> Int -> Maybe (Word8, Int)
        genIntensityTable v idx
          | idx >= 256 = Nothing
          | otherwise =
              let j :: Int = truncate (fromIntegral idx * v)
                  j' = if j > 255 then 255 else j
              in Just (fromIntegral j', idx + 1)

glTextureMode :: B.ByteString -> Quake ()
glTextureMode str = do
    let found = V.find (\m -> BC.map toUpper (m^.glmName) == str) modes

    case found of
      Nothing -> VID.printf Constants.printAll ("bad filter name: [" `B.append` str `B.append` "]\n")
      Just mode -> do
        fastRenderAPIGlobals.frGLFilterMin .= (mode^.glmMinimize)
        fastRenderAPIGlobals.frGLFilterMax .= (mode^.glmMaximize)

        -- change all existing mipmap texture objects
        numGLTextures <- use $ fastRenderAPIGlobals.frNumGLTextures
        changeMipMap mode 0 numGLTextures

  where changeMipMap :: GLModeT -> Int -> Int -> Quake ()
        changeMipMap mode idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just gltRef <- preuse $ fastRenderAPIGlobals.frGLTextures.ix idx
              glt <- io $ readIORef gltRef

              when ((glt^.iType) /= RenderAPIConstants.itPic && (glt^.iType) /= RenderAPIConstants.itSky) $ do
                glBind (glt^.iTexNum)
                GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral $ mode^.glmMinimize)
                GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral $ mode^.glmMaximize)

glTextureAlphaMode :: B.ByteString -> Quake ()
glTextureAlphaMode str = do
    let strUp = BC.map toUpper str
        found = V.find (\m -> BC.map toUpper (m^.gltmName) == strUp) glAlphaModes

    case found of
      Nothing -> VID.printf Constants.printAll ("bad alpha texture mode name: [" `B.append` str `B.append` "]\n")
      Just mode -> fastRenderAPIGlobals.frGLTexAlphaFormat .= (mode^.gltmMode)

glTextureSolidMode :: B.ByteString -> Quake ()
glTextureSolidMode str = do
    let strUp = BC.map toUpper str
        found = V.find (\m -> BC.map toUpper (m^.gltmName) == strUp) glSolidModes

    case found of
      Nothing -> VID.printf Constants.printAll ("bad solid texture mode name: [" `B.append` str `B.append` "]\n")
      Just mode -> fastRenderAPIGlobals.frGLTexSolidFormat .= (mode^.gltmMode)

glTexEnv :: GL.GLenum -> Quake ()
glTexEnv mode = do
    tmu <- use $ fastRenderAPIGlobals.frGLState.glsCurrentTmu
    lastMode <- if tmu == 0
                  then use $ fastRenderAPIGlobals.frLastModes._1
                  else use $ fastRenderAPIGlobals.frLastModes._2

    when (mode /= fromIntegral lastMode) $ do
      GL.glTexEnvi GL.gl_TEXTURE_ENV GL.gl_TEXTURE_ENV_MODE (fromIntegral mode)
      if tmu == 0
        then fastRenderAPIGlobals.frLastModes._1 .= fromIntegral mode
        else fastRenderAPIGlobals.frLastModes._2 .= fromIntegral mode

glSetTexturePalette :: UV.Vector Int -> Quake ()
glSetTexturePalette palette = do
    colorTable <- use $ fastRenderAPIGlobals.frColorTableEXT
    palettedTextureValue <- liftM (^.cvValue) glExtPalettedTextureCVar

    when (colorTable && palettedTextureValue /= 0) $ do
      io (putStrLn "Image.glSetTexturePalette") >> undefined -- TODO

glBind :: Int -> Quake ()
glBind texNum = do
    noBindValue <- liftM (^.cvValue) glNoBindCVar
    drawChars <- use $ fastRenderAPIGlobals.frDrawChars

    texNum' <- if noBindValue /= 0 && isJust drawChars
                 then do
                   -- performance evaluation option
                   img <- io $ readIORef (fromJust drawChars)
                   return (img^.iTexNum)
                 else
                   return texNum

    glState <- use $ fastRenderAPIGlobals.frGLState
    let access = if glState^.glsCurrentTmu == 0
                   then _1
                   else _2

    unless (glState^.glsCurrentTextures.access == texNum') $ do
      if glState^.glsCurrentTmu == 0
        then fastRenderAPIGlobals.frGLState.glsCurrentTextures._1 .= texNum'
        else fastRenderAPIGlobals.frGLState.glsCurrentTextures._2 .= texNum'
      GL.glBindTexture GL.gl_TEXTURE_2D (fromIntegral texNum')

{-
================
GL_LoadPic
This is also used as an entry point for the generated r_notexture
================
-}
glLoadPic :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> Int -> Quake (IORef ImageT)
glLoadPic name pic width height picType bits = do
    -- find a free image_t
    numGLTextures <- use $ fastRenderAPIGlobals.frNumGLTextures
    glTextures <- use $ fastRenderAPIGlobals.frGLTextures

    idx <- findFreeImage glTextures 0 numGLTextures

    when (idx == numGLTextures) $ do
      when (numGLTextures == RenderAPIConstants.maxGLTextures) $
        Com.comError Constants.errDrop "MAX_GLTEXTURES"

      fastRenderAPIGlobals.frNumGLTextures += 1

    when (B.length name > Constants.maxQPath) $
      Com.comError Constants.errDrop ("Draw_LoadPic: \"" `B.append` name `B.append` "\" is too long")

    rs <- use $ fastRenderAPIGlobals.frRegistrationSequence

    let imageRef = glTextures V.! idx

    io $ modifyIORef' imageRef (\v -> v { _iName                 = name
                                        , _iRegistrationSequence = rs
                                        , _iWidth                = width
                                        , _iHeight               = height
                                        , _iType                 = picType
                                        })

    image <- if picType == RenderAPIConstants.itSkin && bits == 8
               then rFloodFillSkin pic width height
               else return pic

    -- TODO: skipped scrap_allocblock and stuff related to scrap here
    --       just using the normal upload routine just like if
    --       Scrap_AllocBlock returned -1

    -- this was label nonscrap
    let texNum = RenderAPIConstants.texNumImages + idx

    io $ modifyIORef' imageRef (\v -> v { _iScrap = False
                                        , _iTexNum = texNum
                                        })

    glBind texNum

    hasAlpha <- if bits == 8
                  then glUpload8  image width height (picType /= RenderAPIConstants.itPic && picType /= RenderAPIConstants.itSky) (picType == RenderAPIConstants.itSky)
                  else glUpload32 image width height (picType /= RenderAPIConstants.itPic && picType /= RenderAPIConstants.itSky)

    uploadWidth <- use $ fastRenderAPIGlobals.frUploadWidth
    uploadHeight <- use $ fastRenderAPIGlobals.frUploadHeight
    uploadedPaletted <- use $ fastRenderAPIGlobals.frUploadedPaletted

    io $ modifyIORef' imageRef (\v -> v { _iHasAlpha     = hasAlpha
                                        , _iUploadWidth  = uploadWidth
                                        , _iUploadHeight = uploadHeight
                                        , _iPaletted     = uploadedPaletted
                                        , _iSL           = 0
                                        , _iSH           = 1
                                        , _iTL           = 0
                                        , _iTH           = 1
                                        })

{-
    -- load little pics into the scrap
    if picType == RenderAPIConstants.itPic && bits == 8 && width < 64 && height < 64
      then do
        io (putStrLn "Image.glLoadPic [scrap]") >> undefined -- TODO
      else do
        -- this was label nonscrap
        let texNum = RenderAPIConstants.texNumImages + idx
          
        zoom (fastRenderAPIGlobals.frGLTextures.ix idx) $ do
          iScrap .= False
          iTexNum .= texNum
        glBind texNum
        hasAlpha <- if bits == 8
                      then glUpload8  image width height (picType /= RenderAPIConstants.itPic && picType /= RenderAPIConstants.itSky) (picType == RenderAPIConstants.itSky)
                      else glUpload32 image width height (picType /= RenderAPIConstants.itPic && picType /= RenderAPIConstants.itSky)
        uploadWidth <- use $ fastRenderAPIGlobals.frUploadWidth
        uploadHeight <- use $ fastRenderAPIGlobals.frUploadHeight
        uploadedPaletted <- use $ fastRenderAPIGlobals.frUploadedPaletted
        zoom (fastRenderAPIGlobals.frGLTextures.ix idx) $ do
          iHasAlpha .= hasAlpha
          iUploadWidth .= uploadWidth
          iUploadHeight .= uploadHeight
          iPaletted .= uploadedPaletted
          iSL .= 0
          iSH .= 1
          iTL .= 0
          iTH .= 1
          -}

    return imageRef

  where findFreeImage :: V.Vector (IORef ImageT) -> Int -> Int -> Quake Int
        findFreeImage glTextures idx maxIdx
          | idx >= maxIdx = return idx
          | otherwise = do
              img <- io $ readIORef (glTextures V.! idx)
              if img^.iTexNum == 0
                then return idx
                else findFreeImage glTextures (idx + 1) maxIdx

rFloodFillSkin :: B.ByteString -> Int -> Int -> Quake B.ByteString
rFloodFillSkin skin skinWidth skinHeight = do
    let fillColor = fromIntegral (skin `B.index` 0) -- assume this is the pixel to fill
    d8to24table <- use $ fastRenderAPIGlobals.frd8to24table
    -- attempt to find opaque black
    let filledColor = fromMaybe 0 (UV.findIndex (== 0xFF000000) d8to24table)

    if fillColor == filledColor || fillColor == 255
      then 
        -- can't fill to filled color or to transparent color (used as visited marker)
        return skin
      else do
        let skinMutable :: MSV.IOVector Word8 = case skin of
                                                 BI.PS ptr 0 n -> MSV.MVector n ptr
                                                 _ -> undefined -- shouldn't happen
        fifo :: MV.IOVector (Int, Int) <- io $ MV.replicate floodFillFifoSize (0, 0)

        io $ floodFill skinMutable fifo (fromIntegral filledColor) (fromIntegral fillColor) 1 0

        return $ case skinMutable of MSV.MVector n ptr -> BI.PS ptr 0 n

  where floodFill :: MSV.IOVector Word8 -> MV.IOVector (Int, Int) -> Word8 -> Word8 -> Int -> Int -> IO ()
        floodFill skinMutable fifo filledColor fillColor inpt outpt
          | inpt == outpt = return ()
          | otherwise = do
              (x, y) <- MV.read fifo outpt
              let fdc = filledColor
                  pos = x + skinWidth * y
                  outpt' = (outpt + 1) .&. floodFillFifoMask

              (inpt1, fdc1) <- if x > 0
                                 then floodFillStep skinMutable fifo inpt pos x y fillColor fdc (-1) (-1) 0
                                 else return (inpt, fdc)

              (inpt2, fdc2) <- if x < skinWidth - 1
                                 then floodFillStep skinMutable fifo inpt1 pos x y fillColor fdc1 1 1 0
                                 else return (inpt1, fdc1)

              (inpt3, fdc3) <- if y > 0
                                 then floodFillStep skinMutable fifo inpt2 pos x y fillColor fdc2 (-skinWidth) 0 (-1)
                                 else return (inpt2, fdc2)

              (inpt4, fdc4) <- if y < skinHeight - 1
                                 then floodFillStep skinMutable fifo inpt3 pos x y fillColor fdc3 skinWidth 0 1
                                 else return (inpt3, fdc3)

              MSV.write skinMutable (x + skinWidth * y) fdc4
              floodFill skinMutable fifo filledColor fillColor inpt4 outpt'

        floodFillStep :: MSV.IOVector Word8 -> MV.IOVector (Int, Int) -> Int -> Int -> Int -> Int -> Word8 -> Word8 -> Int -> Int -> Int -> IO (Int, Word8)
        floodFillStep skinMutable fifo inpt pos x y fillColor fdc off dx dy = do
          b <- MSV.read skinMutable (pos + off)

          if | b == fillColor -> do
               MSV.write skinMutable (pos + off) 255
               MV.write fifo inpt (x + dx, y + dy)
               return ((inpt + 1) .&. floodFillFifoMask, fdc)

             | b /= 255 -> return (inpt, b)

             | otherwise -> return (inpt, fdc)

glUpload8 :: B.ByteString -> Int -> Int -> Bool -> Bool -> Quake Bool
glUpload8 image width height mipmap isSky = do
    let s = width * height

    when (s > 512 * 256) $
      Com.comError Constants.errDrop "GL_Upload8: too large"

    colorTable <- use $ fastRenderAPIGlobals.frColorTableEXT
    palettedTextureValue <- liftM (^.cvValue) glExtPalettedTextureCVar

    if colorTable && palettedTextureValue /= 0 && isSky
      then do
        io $ BU.unsafeUseAsCString image $ \ptr ->
          GL.glTexImage2D GL.gl_TEXTURE_2D
                          0
                          (fromIntegral GL.gl_COLOR_INDEX8_EXT)
                          (fromIntegral width)
                          (fromIntegral height)
                          0
                          GL.gl_COLOR_INDEX
                          GL.gl_UNSIGNED_BYTE
                          ptr

        filterMax <- use $ fastRenderAPIGlobals.frGLFilterMax

        GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral filterMax)
        GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral filterMax)

        -- TODO check this (jake2 comment)
        return False
      else do
        d8to24table <- use $ fastRenderAPIGlobals.frd8to24table
        let trans = constructTrans d8to24table 0 s mempty
        glUpload32 trans width height mipmap

  where constructTrans :: UV.Vector Int -> Int -> Int -> BB.Builder -> B.ByteString
        constructTrans d8to24table idx maxIdx acc
          | idx >= maxIdx = BL.toStrict $ BB.toLazyByteString acc
          | otherwise =
              let !p = image `B.index` idx
                  !t = d8to24table UV.! (fromIntegral p)
                  !p' = if p == 0xFF
                          -- transparent, so scan around for another color
                          -- to avoid alpha fringes
                          -- FIXME: do a full flood fill so mips work...
                          then if | idx > width && (image `B.index` (idx - width)) /= 0xFF -> image `B.index` (idx - width)
                                  | idx < maxIdx - width && (image `B.index` (idx + width)) /= 0xFF -> image `B.index` (idx + width)
                                  | idx > 0 && (image `B.index` (idx - 1)) /= 0xFF -> image `B.index` (idx - 1)
                                  | idx < maxIdx - 1 && (image `B.index` (idx + 1)) /= 0xFF -> image `B.index` (idx + 1)
                                  | otherwise -> 0
                          else p
                  (!t') :: Int32 = fromIntegral $ if p == 0xFF
                                                    -- copy rgb components
                                                    then (d8to24table UV.! (fromIntegral p')) .&. 0x00FFFFFF
                                                    else t
              in constructTrans d8to24table (idx + 1) maxIdx (acc `mappend` (BB.int32LE t'))

glUpload32 :: B.ByteString -> Int -> Int -> Bool -> Quake Bool
glUpload32 image width height mipmap = do
    scaledWidth <- calcScaledWidthHeight width
    scaledHeight <- calcScaledWidthHeight height

    zoom (fastRenderAPIGlobals) $ do
      frUploadedPaletted .= False
      frUploadWidth .= scaledWidth
      frUploadHeight .= scaledHeight

    when (scaledWidth * scaledHeight > 256 * 256) $
      Com.comError Constants.errDrop "GL_Upload32: too big"

    -- scan the texture for any non-255 alpha
    let c = width * height
        samples = scanAlpha 0 3 c

    comp <- if | samples == glSolidFormat -> use $ fastRenderAPIGlobals.frGLTexSolidFormat
               | samples == glAlphaFormat -> use $ fastRenderAPIGlobals.frGLTexAlphaFormat
               | otherwise -> do
                   VID.printf Constants.printAll ("Unknown number of texture components " `B.append` (BC.pack $ show samples) `B.append` "\n") -- IMPROVE?
                   return samples

    result <- if scaledWidth == width && scaledHeight == height
                then
                  if not mipmap
                    then do
                      uploadImage image scaledWidth scaledHeight 0 samples comp
                      -- goto done;
                      return Nothing
                    else
                      return (Just image)
                else do
                  let scaled = glResampleTexture image width height scaledWidth scaledHeight
                  return $ Just scaled

    when (isJust result) $ do
      scaled <- glLightScaleTexture (fromJust result) scaledWidth scaledHeight (not mipmap)
      --when (scaledWidth == 256 && scaledHeight == 128) $ do
        --io $ mapM_ (\i -> printf "0x%02X " (scaled `B.index` i)) [0..10000]

      uploadImage scaled scaledWidth scaledHeight 0 samples comp

      when mipmap $
        uploadMipMaps scaled scaledWidth scaledHeight 0 samples comp

    -- label done

    filterMin <- use $ fastRenderAPIGlobals.frGLFilterMin
    filterMax <- use $ fastRenderAPIGlobals.frGLFilterMax

    if mipmap
      then do
        GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral filterMin)
        GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral filterMax)
      else do
        GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral filterMax)
        GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral filterMax)

    return $ samples == glAlphaFormat

  where calcScaledWidthHeight :: Int -> Quake Int
        calcScaledWidthHeight value = do
          roundDownValue <- liftM (^.cvValue) glRoundDownCVar

          let sv = calcScale 1 value
              sv' = if roundDownValue > 0 && sv > value && mipmap
                      then sv `shiftR` 1
                      else sv

          -- let people sample down the world textures for speed
          sv'' <- if mipmap
                    then do
                      picMipValue <- liftM (truncate . (^.cvValue)) glPicMipCVar
                      return $ sv' `shiftR` picMipValue
                    else return sv'

          -- don't ever bother with > 256 textures
          return $ if | sv'' > 256 -> 256
                      | sv'' < 1 -> 1
                      | otherwise -> sv''

        calcScale :: Int -> Int -> Int
        calcScale v value
          | v >= value = v
          | otherwise = calcScale (v `shiftL` 1) value

        scanAlpha :: Int -> Int -> Int -> Int
        scanAlpha idx scan maxIdx
          | idx >= maxIdx = glSolidFormat
          | otherwise =
              if image `B.index` scan /= 0xFF
                then glAlphaFormat
                else scanAlpha (idx + 1) (scan + 4) maxIdx

        uploadImage :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> Quake ()
        uploadImage img scaledWidth scaledHeight mipLevel samples comp = do
          colorTable <- use $ fastRenderAPIGlobals.frColorTableEXT
          palettedTextureValue <- liftM (^.cvValue) glExtPalettedTextureCVar

          if colorTable && palettedTextureValue /= 0 && samples == glSolidFormat
            then do
              fastRenderAPIGlobals.frUploadedPaletted .= True
              palettedTexture <- glBuildPalettedTexture img scaledWidth scaledHeight
              io $ BU.unsafeUseAsCString palettedTexture $ \ptr ->
                GL.glTexImage2D GL.gl_TEXTURE_2D
                                (fromIntegral mipLevel) 
                                (fromIntegral GL.gl_COLOR_INDEX8_EXT)
                                (fromIntegral scaledWidth)
                                (fromIntegral scaledHeight)
                                0
                                GL.gl_COLOR_INDEX
                                GL.gl_UNSIGNED_BYTE
                                ptr

            else do
              let img' = img `B.append` "\0"
              io $ BU.unsafeUseAsCString img' $ \ptr ->
                GL.glTexImage2D GL.gl_TEXTURE_2D
                                (fromIntegral mipLevel)
                                (fromIntegral comp)
                                (fromIntegral scaledWidth)
                                (fromIntegral scaledHeight)
                                0
                                GL.gl_RGBA
                                GL.gl_UNSIGNED_BYTE
                                ptr

        uploadMipMaps :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> Quake ()
        uploadMipMaps img scaledWidth scaledHeight mipLevel samples comp
          | scaledWidth <= 1 && scaledHeight <= 1 = return ()
          | otherwise = do
              let scaled = glMipMap img scaledWidth scaledHeight
                  sw = scaledWidth `shiftR` 1
                  sh = scaledHeight `shiftR` 1
                  scaledWidth' = if sw < 1 then 1 else sw
                  scaledHeight' = if sh < 1 then 1 else sh
                  mipLevel' = mipLevel + 1

              uploadImage scaled scaledWidth' scaledHeight' mipLevel' samples comp
              uploadMipMaps scaled scaledWidth' scaledHeight' mipLevel' samples comp

glBuildPalettedTexture :: B.ByteString -> Int -> Int -> Quake B.ByteString
glBuildPalettedTexture _ _ _ = io (putStrLn "Image.glBuildPalettedTexture") >> undefined -- TODO

glResampleTexture :: B.ByteString -> Int -> Int -> Int -> Int -> B.ByteString
glResampleTexture img width height scaledWidth scaledHeight =
    let fracStep = (width * 0x10000) `div` scaledWidth
        frac = fracStep `shiftR` 2
        p1 = UV.unfoldr buildP (0, frac, fracStep)
        frac' = 3 * (fracStep `shiftR` 2)
        p2 = UV.unfoldr buildP (0, frac', fracStep)
    in resample fracStep p1 p2 0 scaledHeight mempty

  where buildP :: (Int, Int, Int) -> Maybe (Int, (Int, Int, Int))
        buildP (idx, frac, fracStep)
          | idx >= scaledWidth = Nothing
          | otherwise = let v = 4 * (frac `shiftR` 16)
                        in Just (v, (idx + 1, frac + fracStep, fracStep))

        resample :: Int -> UV.Vector Int -> UV.Vector Int -> Int -> Int -> BB.Builder -> B.ByteString
        resample fracStep p1 p2 idx maxIdx acc
          | idx >= maxIdx = BL.toStrict $ BB.toLazyByteString acc
          | otherwise =
              let inRow = 4 * width * truncate ((fromIntegral idx + 0.25 :: Float) * fromIntegral height / fromIntegral scaledHeight)
                  inRow2 = 4 * width * truncate ((fromIntegral idx + 0.75 :: Float) * fromIntegral height / fromIntegral scaledHeight)
                  row = buildRow p1 p2 inRow inRow2 0 scaledWidth mempty
              in resample fracStep p1 p2 (idx + 1) maxIdx (acc `mappend` row)

        buildRow :: UV.Vector Int -> UV.Vector Int -> Int -> Int -> Int -> Int -> BB.Builder -> BB.Builder
        buildRow p1 p2 inRow inRow2 idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let !pix1 = inRow  + (p1 UV.! idx)
                  !pix2 = inRow  + (p2 UV.! idx)
                  !pix3 = inRow2 + (p1 UV.! idx)
                  !pix4 = inRow2 + (p2 UV.! idx)
                  r1 :: Int = fromIntegral (img `B.index` (pix1 + 0))
                  g1 :: Int = fromIntegral (img `B.index` (pix1 + 1))
                  b1 :: Int = fromIntegral (img `B.index` (pix1 + 2))
                  a1 :: Int = fromIntegral (img `B.index` (pix1 + 3))
                  r :: Word8 = fromIntegral $ (r1 + fromIntegral (img `B.index` (pix2 + 0)) + fromIntegral (img `B.index` (pix3 + 0)) + fromIntegral (img `B.index` (pix4 + 0))) `shiftR` 2
                  g :: Word8 = fromIntegral $ (g1 + fromIntegral (img `B.index` (pix2 + 1)) + fromIntegral (img `B.index` (pix3 + 1)) + fromIntegral (img `B.index` (pix4 + 1))) `shiftR` 2
                  b :: Word8 = fromIntegral $ (b1 + fromIntegral (img `B.index` (pix2 + 2)) + fromIntegral (img `B.index` (pix3 + 2)) + fromIntegral (img `B.index` (pix4 + 2))) `shiftR` 2
                  a :: Word8 = fromIntegral $ (a1 + fromIntegral (img `B.index` (pix2 + 3)) + fromIntegral (img `B.index` (pix3 + 3)) + fromIntegral (img `B.index` (pix4 + 3))) `shiftR` 2
              -- in buildRow p1 p2 inRow inRow2 (idx + 1) maxIdx (acc `mappend` (mconcat (fmap BB.word8 [r, g, b, a])))
              in buildRow p1 p2 inRow inRow2 (idx + 1) maxIdx (acc `mappend` BB.word8 r `mappend` BB.word8 g `mappend` BB.word8 b `mappend` BB.word8 a)

{-
================
GL_LightScaleTexture
Scale up the pixel values in a texture to increase the
lighting range
================
-}
glLightScaleTexture :: B.ByteString -> Int -> Int -> Bool -> Quake B.ByteString
glLightScaleTexture img width height onlyGamma = do
    let c = width * height
    gammaTable <- use $ fastRenderAPIGlobals.frGammaTable

    if onlyGamma
      then return $ buildFromGammaTable gammaTable 0 0 c mempty
      else do
        intensityTable <- use $ fastRenderAPIGlobals.frIntensityTable
        return $ buildFromGammaAndIntesityTable gammaTable intensityTable 0 0 c mempty

  where buildFromGammaTable :: B.ByteString -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        buildFromGammaTable gammaTable idx p maxIdx acc
          | idx >= maxIdx = BL.toStrict $ BB.toLazyByteString acc
          | otherwise =
              let !p0 = img `B.index` p
                  !p1 = img `B.index` (p + 1)
                  !p2 = img `B.index` (p + 2)
                  !p3 = img `B.index` (p + 3)
                  !a = gammaTable `B.index` (fromIntegral p0)
                  !b = gammaTable `B.index` (fromIntegral p1)
                  !c = gammaTable `B.index` (fromIntegral p2)
              in buildFromGammaTable gammaTable (idx + 1) (p + 4) maxIdx (acc `mappend` BB.word8 a `mappend` BB.word8 b `mappend` BB.word8 c `mappend` BB.word8 p3)

        buildFromGammaAndIntesityTable :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        buildFromGammaAndIntesityTable gammaTable intensityTable idx p maxIdx acc
          | idx >= maxIdx = BL.toStrict $ BB.toLazyByteString acc
          | otherwise =
              let !p0 = img `B.index` p
                  !p1 = img `B.index` (p + 1)
                  !p2 = img `B.index` (p + 2)
                  !p3 = img `B.index` (p + 3)
                  !i0 = intensityTable `B.index` (fromIntegral p0)
                  !i1 = intensityTable `B.index` (fromIntegral p1)
                  !i2 = intensityTable `B.index` (fromIntegral p2)
                  !a = gammaTable `B.index` (fromIntegral i0)
                  !b = gammaTable `B.index` (fromIntegral i1)
                  !c = gammaTable `B.index` (fromIntegral i2)
              -- in buildFromGammaAndIntesityTable gammaTable intensityTable (idx + 1) (p + 4) maxIdx (acc `mappend` (mconcat (fmap BB.word8 [a, b, c, p3])))
              in buildFromGammaAndIntesityTable gammaTable intensityTable (idx + 1) (p + 4) maxIdx (acc `mappend` BB.word8 a `mappend` BB.word8 b `mappend` BB.word8 c `mappend` BB.word8 p3)

{-
================
GL_MipMap
Operates in place, quartering the size of the texture
================
-}
glMipMap :: B.ByteString -> Int -> Int -> B.ByteString
glMipMap img width height =
    let height' = height `shiftR` 1
    in if width == 1 || height == 1
         then B.replicate (width * height) 255 -- TODO: UGLY HACK!!!!!! FIXME!! mipmap generation fails when w or h is 1 (in jake2 and quake2 it uses the fact that input array is huge and can overflow, but we cannot)
         else forI 0 0 height' mempty

  where forI :: Int -> Int -> Int -> BB.Builder -> B.ByteString
        forI inIdx i maxI acc
          | i >= maxI = BL.toStrict $ BB.toLazyByteString acc
          | otherwise = 
              let w = width `shiftL` 2
                  (inIdx', r) = forJ inIdx 0 w mempty
              in forI (inIdx' + w) (i + 1) maxI (acc `mappend` r)

        forJ :: Int -> Int -> Int -> BB.Builder -> (Int, BB.Builder)
        forJ inIdx j maxJ acc
          | j >= maxJ = (inIdx, acc)
          | otherwise =
              let !w = width `shiftL` 2
                  !a = ((img `B.index` (inIdx + 0)) + (img `B.index` (inIdx + 4)) + (img `B.index` (inIdx + w + 0)) + (img `B.index` (inIdx + w + 4))) `shiftR` 2
                  !b = ((img `B.index` (inIdx + 1)) + (img `B.index` (inIdx + 5)) + (img `B.index` (inIdx + w + 1)) + (img `B.index` (inIdx + w + 5))) `shiftR` 2
                  !c = ((img `B.index` (inIdx + 2)) + (img `B.index` (inIdx + 6)) + (img `B.index` (inIdx + w + 2)) + (img `B.index` (inIdx + w + 6))) `shiftR` 2
                  !d = ((img `B.index` (inIdx + 3)) + (img `B.index` (inIdx + 7)) + (img `B.index` (inIdx + w + 3)) + (img `B.index` (inIdx + w + 7))) `shiftR` 2
              in forJ (inIdx + 8) (j + 8) maxJ (acc `mappend` BB.word8 a `mappend` BB.word8 b `mappend` BB.word8 c `mappend` BB.word8 d)

glFindImage :: B.ByteString -> Int -> Quake (Maybe (IORef ImageT))
glFindImage imgName imgType = do
    if B.length imgName < 1
      then return Nothing
      else do
        -- look for it
        numGLTextures <- use $ fastRenderAPIGlobals.frNumGLTextures
        glTextures <- use $ fastRenderAPIGlobals.frGLTextures

        found <- findImage glTextures 0 numGLTextures

        case found of
          Just imageRef -> do
            registrationSequence <- use $ fastRenderAPIGlobals.frRegistrationSequence
            io $ modifyIORef' imageRef (\v -> v { _iRegistrationSequence = registrationSequence })
            return found
          Nothing -> do
            -- load the pic from disk
            if | ".pcx" `BC.isSuffixOf` imgName -> do
                   (pic, _, dimensions) <- loadPCX imgName False True
                   if isNothing pic
                     then return Nothing
                     else do
                       let Just (width, height) = dimensions
                       imgRef <- glLoadPic imgName (fromJust pic) width height imgType 8
                       return $ Just imgRef
               | ".wal" `BC.isSuffixOf` imgName -> do
                   imgRef <- glLoadWal imgName
                   return $ Just imgRef
               | ".tga" `BC.isSuffixOf` imgName -> do
                   tga <- loadTGA imgName
                   case tga of
                     Nothing -> return Nothing
                     Just (pic, (width, height)) -> do
                       imgRef <- glLoadPic imgName pic width height imgType 32
                       return $ Just imgRef
               | otherwise -> do
                   (pic, _, dimensions) <- loadPCX ("pics/" `B.append` imgName `B.append` ".pcx") False True
                   if isNothing pic
                     then return Nothing
                     else do
                       let Just (width, height) = dimensions
                       imgRef <- glLoadPic imgName (fromJust pic) width height imgType 8
                       return $ Just imgRef

  where findImage :: V.Vector (IORef ImageT) -> Int -> Int -> Quake (Maybe (IORef ImageT))
        findImage textures idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              img <- io $ readIORef (textures V.! idx)
              if img^.iName == imgName
                then return $ Just (textures V.! idx)
                else findImage textures (idx + 1) maxIdx

glLoadWal :: B.ByteString -> Quake (IORef ImageT)
glLoadWal name = do
    raw <- FS.loadFile name

    case raw of
      Nothing -> do
        VID.printf Constants.printAll ("GL_FindImage: can't load " `B.append` name `B.append` "\n")
        use $ fastRenderAPIGlobals.frNoTexture
      Just buf -> do
        let miptexT = newMiptexT (BL.fromStrict buf)
            sz = (miptexT^.mWidth) * (miptexT^.mHeight)
            offset = (miptexT^.mOffsets) UV.! 0
            imgBuf = B.take sz (B.drop offset buf)

        glLoadPic name imgBuf (miptexT^.mWidth) (miptexT^.mHeight) RenderAPIConstants.itWall 8

loadTGA :: B.ByteString -> Quake (Maybe (B.ByteString, (Int, Int)))
loadTGA name = do
    raw <- FS.loadFile name

    case raw of
      Nothing -> do
        VID.printf Constants.printDeveloper ("Bad tga file " `B.append` name `B.append` "\n")
        return Nothing
      Just tgaContents -> do
        let tgaHeader = newTgaT (BL.fromStrict tgaContents)

        when ((tgaHeader^.tgaImageType) /= 2 && (tgaHeader^.tgaImageType) /= 10) $
          Com.comError Constants.errDrop "LoadTGA: Only type 2 and 10 targa RGB images supported\n"

        when ((tgaHeader^.tgaColorMapType) /= 0 || ((tgaHeader^.tgaPixelSize) /= 32 && (tgaHeader^.tgaPixelSize) /= 24)) $
          Com.comError Constants.errDrop "LoadTGA: Only 32 or 24 bit images supported (no colormaps)\n"

        let columns = fromIntegral $ tgaHeader^.tgaWidth
            rows = fromIntegral $ tgaHeader^.tgaHeight
            buf = if (tgaHeader^.tgaIdLength) /= 0
                    then BL.drop (fromIntegral $ tgaHeader^.tgaIdLength) (tgaHeader^.tgaData)
                    else tgaHeader^.tgaData

        if (tgaHeader^.tgaImageType) == 2
          then do -- uncompressed, RGB images
            let pic = readTGA (BL.toStrict buf) (tgaHeader^.tgaPixelSize) 0 0 rows columns mempty
            return $ Just (pic, (rows, columns))
          else do -- tgaImageType == 10 -- runlength encoded RGB images
            io (putStrLn "Image.loadTGA#10") >> undefined -- TODO

  where readTGA :: B.ByteString -> Word8 -> Int -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        readTGA buf pixelSize idx row maxRow maxColumn acc
          | row >= maxRow = BL.toStrict $ BB.toLazyByteString acc
          | otherwise = let tgaRow = readTGARow buf pixelSize idx 0 maxColumn mempty
                        in readTGA buf pixelSize (idx + maxColumn) (row + 1) maxRow maxColumn (tgaRow `mappend` acc)

        readTGARow :: B.ByteString -> Word8 -> Int -> Int -> Int -> BB.Builder -> BB.Builder
        readTGARow buf pixelSize idx column maxColumn acc
          | column >= maxColumn = acc
          | otherwise =
              if pixelSize == 24
                then let b = buf `B.index` (idx + 0)
                         g = buf `B.index` (idx + 1)
                         r = buf `B.index` (idx + 2)
                     --in readTGARow buf pixelSize (idx + 3) (column + 1) maxColumn (acc `mappend` (mconcat (fmap BB.word8 [255, b, g, r])))
                     in readTGARow buf pixelSize (idx + 3) (column + 1) maxColumn (acc `mappend` BB.word8 255 `mappend` BB.word8 b `mappend` BB.word8 g `mappend` BB.word8 r)

                -- pixelSize == 32
                else let b = buf `B.index` (idx + 0)
                         g = buf `B.index` (idx + 1)
                         r = buf `B.index` (idx + 2)
                         a = buf `B.index` (idx + 3)
                     --in readTGARow buf pixelSize (idx + 4) (column + 1) maxColumn (acc `mappend` (mconcat (fmap BB.word8 [a, b, g, r])))
                     in readTGARow buf pixelSize (idx + 4) (column + 1) maxColumn (acc `mappend` BB.word8 a `mappend` BB.word8 b `mappend` BB.word8 g `mappend` BB.word8 r)

scrapUpload :: Quake ()
scrapUpload = io (putStrLn "Image.scrapUpload") >> undefined -- TODO

glEnableMultiTexture :: Bool -> Quake ()
glEnableMultiTexture enable = do
    if enable
      then do
        use (fastRenderAPIGlobals.frTexture1) >>= glSelectTexture
        GL.glEnable GL.gl_TEXTURE_2D
        glTexEnv GL.gl_REPLACE
      else do
        use (fastRenderAPIGlobals.frTexture1) >>= glSelectTexture
        GL.glDisable GL.gl_TEXTURE_2D
        glTexEnv GL.gl_REPLACE

    use (fastRenderAPIGlobals.frTexture0) >>= glSelectTexture
    glTexEnv GL.gl_REPLACE

glSelectTexture :: Int -> Quake ()
glSelectTexture texture = do
    texture0 <- use $ fastRenderAPIGlobals.frTexture0
    glState <- use $ fastRenderAPIGlobals.frGLState

    let tmu = if texture0 == texture then 0 else 1

    when (tmu /= (glState^.glsCurrentTmu)) $ do
      fastRenderAPIGlobals.frGLState.glsCurrentTmu .= tmu
      GL.glActiveTextureARB (fromIntegral texture)
      GL.glClientActiveTextureARB (fromIntegral texture)

rRegisterSkin :: B.ByteString -> Quake (Maybe (IORef ImageT))
rRegisterSkin name = glFindImage name RenderAPIConstants.itSkin

{-
================
GL_FreeUnusedImages
Any image that was not touched on this registration sequence
will be freed.
================
-}
glFreeUnusedImages :: Quake ()
glFreeUnusedImages = do
    -- never free r_notexture or particle texture
    regSeq <- use $ fastRenderAPIGlobals.frRegistrationSequence
    noTextureRef <- use $ fastRenderAPIGlobals.frNoTexture
    particleTextureRef <- use $ fastRenderAPIGlobals.frParticleTexture

    io $ do
      modifyIORef' noTextureRef (\v -> v { _iRegistrationSequence = regSeq })
      modifyIORef' particleTextureRef (\v -> v { _iRegistrationSequence = regSeq })

    numGLTextures <- use $ fastRenderAPIGlobals.frNumGLTextures
    glTextures <- use $ fastRenderAPIGlobals.frGLTextures

    checkImages glTextures regSeq 0 numGLTextures

  where checkImages :: V.Vector (IORef ImageT) -> Int -> Int -> Int -> Quake ()
        checkImages glTextures regSeq idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              image <- io $ readIORef (glTextures V.! idx)

              if (image^.iRegistrationSequence) == regSeq ||
                 (image^.iRegistrationSequence) == 0 ||
                 (image^.iType) == RenderAPIConstants.itPic
                then
                  checkImages glTextures regSeq (idx + 1) maxIdx
                else do
                  io $ with (fromIntegral $ image^.iTexNum) $ \ptr -> do
                    GL.glDeleteTextures 1 ptr
                    writeIORef (glTextures V.! idx) (newImageT idx)
                  checkImages glTextures regSeq (idx + 1) maxIdx

glMBind :: Int -> Int -> Quake ()
glMBind target texNum = do
    glSelectTexture target
    texture0 <- use $ fastRenderAPIGlobals.frTexture0
    glState <- use $ fastRenderAPIGlobals.frGLState

    let done = if target == texture0
              then (glState^.glsCurrentTextures._1) == texNum
              else (glState^.glsCurrentTextures._2) == texNum

    unless done $
      glBind texNum
