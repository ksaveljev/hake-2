{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Image where

import Control.Lens ((^.), (.=), (+=), use, preuse, ix, _1, _2, zoom)
import Control.Monad (when, void, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (toUpper)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import CVarVariables
import QCommon.QFiles.PcxT
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

        particleTGlobals.pColorTable .= UV.map (.&. 0x00FFFFFF) d8to24table

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
            let pix = decodePCX raw 0 0 0 height width ""

            return (Just pix, palette, dimensions)

  where decodePCX :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> B.ByteString -> B.ByteString
        decodePCX raw idx x y maxX maxY acc
          | y >= maxY = acc
          | x >= maxX = decodePCX raw idx 0 (y + 1) maxX maxY acc
          | otherwise =
              let dataByte = B.index raw idx
              in if dataByte .&. 0xC0 == 0xC0
                   then let runLength = fromIntegral $ dataByte .&. 0x3F
                            byte = B.index raw (idx + 1)
                            -- write runLength pixel
                        in decodePCX raw (idx + 2) (x + runLength) y maxX maxY $! acc `B.append` (B.replicate runLength byte)
                   else -- write one pixel
                     decodePCX raw (idx + 1) (x + 1) y maxX maxY $! acc `B.snoc` dataByte

glImageListF :: XCommandT
glImageListF = io (putStrLn "Image.glImageListF") >> undefined -- TODO

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
              Just glt <- preuse $ fastRenderAPIGlobals.frGLTextures.ix idx

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
glSetTexturePalette _ = io (putStrLn "Image.glSetTexturePalette") >> undefined -- TODO

glBind :: Int -> Quake ()
glBind texNum = do
    noBindValue <- liftM (^.cvValue) glNoBindCVar
    drawChars <- use $ fastRenderAPIGlobals.frDrawChars

    texNum' <- if noBindValue /= 0 && isJust drawChars
                 then do
                   -- performance evaluation option
                   let (Just (ImageReference idx)) = drawChars
                   Just num <- preuse $ fastRenderAPIGlobals.frGLTextures.ix idx.iTexNum
                   return num
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
glLoadPic :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> Int -> Quake ImageReference
glLoadPic name pic width height picType bits = do
    -- find a free image_t
    numGLTextures <- use $ fastRenderAPIGlobals.frNumGLTextures
    glTextures <- use $ fastRenderAPIGlobals.frGLTextures

    let imageRef@(ImageReference idx) = findFreeImage glTextures 0 numGLTextures

    when (idx == numGLTextures) $ do
      when (numGLTextures == RenderAPIConstants.maxGLTextures) $
        Com.comError Constants.errDrop "MAX_GLTEXTURES"

      fastRenderAPIGlobals.frNumGLTextures += 1

    when (B.length name > Constants.maxQPath) $
      Com.comError Constants.errDrop ("Draw_LoadPic: \"" `B.append` name `B.append` "\" is too long")

    rs <- use $ fastRenderAPIGlobals.frRegistrationSequence

    zoom (fastRenderAPIGlobals.frGLTextures.ix idx) $ do
      iName .= name
      iRegistrationSequence .= rs
      iWidth .= width
      iHeight .= height
      iType .= picType

    image <- if picType == RenderAPIConstants.itSkin && bits == 8
               then rFloodFillSkin pic width height
               else return pic

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

    return imageRef

  where findFreeImage :: V.Vector ImageT -> Int -> Int -> ImageReference
        findFreeImage glTextures idx maxIdx
          | idx >= maxIdx = ImageReference maxIdx
          | otherwise =
              if (glTextures V.! idx)^.iTexNum == 0
                then ImageReference idx
                else findFreeImage glTextures (idx + 1) maxIdx

rFloodFillSkin :: B.ByteString -> Int -> Int -> Quake B.ByteString
rFloodFillSkin _ _ _ = do
    io (putStrLn "Image.rFloodFillSkin") >> undefined -- TODO

glUpload8 :: B.ByteString -> Int -> Int -> Bool -> Bool -> Quake Bool
glUpload8 _ _ _ _ _ = do
    io (putStrLn "Image.glUpload8") >> undefined -- TODO

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

            else
              io $ BU.unsafeUseAsCString img $ \ptr ->
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
          | scaledWidth <= 1 || scaledHeight <= 1 = return ()
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

{-
================
GL_ResampleTexture
================
-}
glResampleTexture :: B.ByteString -> Int -> Int -> Int -> Int -> B.ByteString
glResampleTexture img width height scaledWidth scaledHeight =
    let fracStep = (width * 0x10000) `div` scaledWidth
        frac = fracStep `shiftR` 2
        p1 = buildP frac fracStep 0 scaledWidth ""
        frac' = 3 * (fracStep `shiftR` 2)
        p2 = buildP frac' fracStep 0 scaledWidth ""
    in resample fracStep p1 p2 0 scaledHeight ""

  where buildP :: Int -> Int -> Int -> Int -> B.ByteString -> B.ByteString
        buildP frac fracStep idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let v = 4 * (frac `shiftR` 16)
              in buildP (frac + fracStep) fracStep (idx + 1) maxIdx (acc `B.snoc` fromIntegral v)

        resample :: Int -> B.ByteString -> B.ByteString -> Int -> Int -> B.ByteString -> B.ByteString
        resample fracStep p1 p2 idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              -- TODO: make sure we need '4 *' here
              let inRow = 4 * width * truncate ((fromIntegral idx + 0.25 :: Float) * fromIntegral height / fromIntegral scaledHeight)
                  inRow2 = 4 * width * truncate ((fromIntegral idx + 0.75 :: Float) * fromIntegral height / fromIntegral scaledHeight)
                  -- frac = fracStep `shiftR` 1
                  row = buildRow p1 p2 inRow inRow2 0 scaledWidth ""
              in resample fracStep p1 p2 (idx + 1) maxIdx (acc `B.append` row)

        buildRow :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> Int -> B.ByteString -> B.ByteString
        buildRow p1 p2 inRow inRow2 idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let pix1 = inRow  + fromIntegral (p1 `B.index` idx)
                  pix2 = inRow  + fromIntegral (p2 `B.index` idx)
                  pix3 = inRow2 + fromIntegral (p1 `B.index` idx)
                  pix4 = inRow2 + fromIntegral (p2 `B.index` idx)
                  a = ((img `B.index` (pix1 + 0)) + (img `B.index` (pix2 + 0)) + (img `B.index` (pix3 + 0)) + (img `B.index` (pix4 + 0))) `shiftR` 2
                  b = ((img `B.index` (pix1 + 1)) + (img `B.index` (pix2 + 1)) + (img `B.index` (pix3 + 1)) + (img `B.index` (pix4 + 1))) `shiftR` 2
                  c = ((img `B.index` (pix1 + 2)) + (img `B.index` (pix2 + 2)) + (img `B.index` (pix3 + 2)) + (img `B.index` (pix4 + 2))) `shiftR` 2
                  d = ((img `B.index` (pix1 + 3)) + (img `B.index` (pix2 + 3)) + (img `B.index` (pix3 + 3)) + (img `B.index` (pix4 + 3))) `shiftR` 2
              in buildRow p1 p2 inRow inRow2 (idx + 1) maxIdx (acc `B.append` (B.pack [a, b, c, d]))

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
      then return $ buildFromGammaTable gammaTable 0 0 c ""
      else do
        intensityTable <- use $ fastRenderAPIGlobals.frIntensityTable
        return $ buildFromGammaAndIntesityTable gammaTable intensityTable 0 0 c ""

  where buildFromGammaTable :: B.ByteString -> Int -> Int -> Int -> B.ByteString -> B.ByteString
        buildFromGammaTable gammaTable idx p maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let p0 = img `B.index` p
                  p1 = img `B.index` (p + 1)
                  p2 = img `B.index` (p + 2)
                  p3 = img `B.index` (p + 3)
                  a = gammaTable `B.index` (fromIntegral p0)
                  b = gammaTable `B.index` (fromIntegral p1)
                  c = gammaTable `B.index` (fromIntegral p2)
              in buildFromGammaTable gammaTable (idx + 1) (p + 4) maxIdx (acc `B.append` (B.pack [a, b, c, p3]))

        buildFromGammaAndIntesityTable :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> B.ByteString -> B.ByteString
        buildFromGammaAndIntesityTable gammaTable intensityTable idx p maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let p0 = img `B.index` p
                  p1 = img `B.index` (p + 1)
                  p2 = img `B.index` (p + 2)
                  p3 = img `B.index` (p + 3)
                  i0 = intensityTable `B.index` (fromIntegral p0)
                  i1 = intensityTable `B.index` (fromIntegral p1)
                  i2 = intensityTable `B.index` (fromIntegral p2)
                  a = gammaTable `B.index` (fromIntegral i0)
                  b = gammaTable `B.index` (fromIntegral i1)
                  c = gammaTable `B.index` (fromIntegral i2)
              in buildFromGammaAndIntesityTable gammaTable intensityTable (idx + 1) (p + 4) maxIdx (acc `B.append` (B.pack [a, b, c, p3]))

{-
================
GL_MipMap

Operates in place, quartering the size of the texture
================
-}
glMipMap :: B.ByteString -> Int -> Int -> B.ByteString
glMipMap img width height =
    let height' = height `shiftR` 1
    in forI 0 0 height' ""

  where forI :: Int -> Int -> Int -> B.ByteString -> B.ByteString
        forI inIdx i maxI acc
          | i >= maxI = acc
          | otherwise = 
              let w = width `shiftL` 2
                  (inIdx', r) = forJ inIdx 0 w ""
              in forI (inIdx' + w) (i + 1) maxI (acc `B.append` r)

        forJ :: Int -> Int -> Int -> B.ByteString -> (Int, B.ByteString)
        forJ inIdx j maxJ acc
          | j >= maxJ = (inIdx, acc)
          | otherwise =
              let w = width `shiftL` 2
                  a = ((img `B.index` (inIdx + 0)) + (img `B.index` (inIdx + 4)) + (img `B.index` (inIdx + w + 0)) + (img `B.index` (inIdx + w + 4))) `shiftR` 2
                  b = ((img `B.index` (inIdx + 1)) + (img `B.index` (inIdx + 5)) + (img `B.index` (inIdx + w + 1)) + (img `B.index` (inIdx + w + 5))) `shiftR` 2
                  c = ((img `B.index` (inIdx + 2)) + (img `B.index` (inIdx + 6)) + (img `B.index` (inIdx + w + 2)) + (img `B.index` (inIdx + w + 6))) `shiftR` 2
                  d = ((img `B.index` (inIdx + 3)) + (img `B.index` (inIdx + 7)) + (img `B.index` (inIdx + w + 3)) + (img `B.index` (inIdx + w + 7))) `shiftR` 2
              in forJ (inIdx + 8) (j + 8) maxJ (acc `B.append` (B.pack [a, b, c, d]))

glFindImage :: B.ByteString -> Int -> Quake (Maybe ImageReference)
glFindImage _ _ = do
    io (putStrLn "Image.glFindImage") >> undefined -- TODO
