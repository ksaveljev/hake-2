{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Image
  ( getPalette
  , glBind
  , glFindImage
  , glImageListF
  , glInitImages
  , glLoadPic
  , glSetTexturePalette
  , glShutdownImages
  , glTexEnv
  , glTextureMode
  , glTextureAlphaMode
  , glTextureSolidMode
  , rRegisterSkin
  ) where

import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import           QCommon.MiptexT
import           QCommon.QFiles.PcxT
import           QuakeIOState
import           QuakeRef
import           QuakeState
import           Render.GLConfigT
import           Render.GLModeT
import           Render.GLStateT
import           Render.GLTModeT
import           Render.ImageT
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, zoom, (^.), (.=), (+=), _1, _2, (&), (.~))
import           Control.Monad (void, when)
import           Control.Monad.Coroutine (mapMonad)
import           Control.Monad.State.Strict (runStateT)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import           Data.Char (toUpper)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import           Data.Word (Word8)
import           Foreign.Marshal.Utils (with)
import qualified Graphics.GL as GL
import qualified Pipes.Binary as PB
import qualified Pipes.ByteString as PBS
import           System.IO (Handle)

glSolidFormat :: Int
glSolidFormat = 3

glAlphaFormat :: Int
glAlphaFormat = 4

floodFillFifoMask :: Int
floodFillFifoMask = floodFillFifoSize - 1

modes :: V.Vector GLModeT
modes = V.fromList
  [ GLModeT "GL_NEAREST"                (fromIntegral GL.GL_NEAREST               ) (fromIntegral GL.GL_NEAREST)
  , GLModeT "GL_LINEAR"                 (fromIntegral GL.GL_LINEAR                ) (fromIntegral GL.GL_LINEAR )
  , GLModeT "GL_NEAREST_MIPMAP_NEAREST" (fromIntegral GL.GL_NEAREST_MIPMAP_NEAREST) (fromIntegral GL.GL_NEAREST)
  , GLModeT "GL_LINEAR_MIPMAP_NEAREST"  (fromIntegral GL.GL_LINEAR_MIPMAP_NEAREST ) (fromIntegral GL.GL_LINEAR )
  , GLModeT "GL_NEAREST_MIPMAP_LINEAR"  (fromIntegral GL.GL_NEAREST_MIPMAP_LINEAR ) (fromIntegral GL.GL_NEAREST)
  , GLModeT "GL_LINEAR_MIPMAP_LINEAR"   (fromIntegral GL.GL_LINEAR_MIPMAP_LINEAR  ) (fromIntegral GL.GL_LINEAR )
  ]

glAlphaModes :: V.Vector GLTModeT
glAlphaModes = V.fromList
  [ GLTModeT "default"    4
  , GLTModeT "GL_RGBA"    (fromIntegral GL.GL_RGBA   )
  , GLTModeT "GL_RGBA8"   (fromIntegral GL.GL_RGBA8  )
  , GLTModeT "GL_RGB5_A1" (fromIntegral GL.GL_RGB5_A1)
  , GLTModeT "GL_RGBA4"   (fromIntegral GL.GL_RGBA4  )
  , GLTModeT "GL_RGBA2"   (fromIntegral GL.GL_RGBA2  )
  ]

glSolidModes :: V.Vector GLTModeT
glSolidModes = V.fromList
  [ GLTModeT "default"     3
  , GLTModeT "GL_RGB"      (fromIntegral GL.GL_RGB     )
  , GLTModeT "GL_RGB8"     (fromIntegral GL.GL_RGB8    )
  , GLTModeT "GL_RGB5"     (fromIntegral GL.GL_RGB5    )
  , GLTModeT "GL_RGB4"     (fromIntegral GL.GL_RGB4    )
  , GLTModeT "GL_R3_G3_B2" (fromIntegral GL.GL_R3_G3_B2)
  ]

rRegisterSkin :: B.ByteString -> Quake (Maybe (Ref ImageT))
rRegisterSkin name = glFindImage name Constants.itSkin

glShutdownImages :: Quake ()
glShutdownImages = clearTextures 0 =<< use (fastRenderAPIGlobals.frNumGLTextures)

clearTextures :: Int -> Int -> Quake ()
clearTextures idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise =
      do image <- readRef (Ref idx)
         clearWithRegSeq (image^.iTexNum) (image^.iRegistrationSequence)
         clearTextures (idx + 1) maxIdx
  where clearWithRegSeq _ 0 = return ()
        clearWithRegSeq texNum _ =
          do request (deleteTexture texNum)
             writeRef (Ref idx) (newImageT idx)
        deleteTexture texNum = io (with (fromIntegral texNum) (GL.glDeleteTextures 1))

getPalette :: Quake ()
getPalette =
  do imageData <- loadPCX "pics/colormap.pcx" True False
     proceedConstructPalette imageData
  where proceedConstructPalette Nothing = paletteError
        proceedConstructPalette (Just (_, palette, _))
          | B.length palette /= 768 = paletteError
          | otherwise = buildPalette palette
        paletteError = Com.fatalError "Couldn't load pics/colormap.pcx"

buildPalette :: B.ByteString -> Quake ()
buildPalette palette =
  do fastRenderAPIGlobals.frd8to24table .= d8to24table
     particleTGlobals.pColorTable .= UV.map ((.&. 0x00FFFFFF) . fromIntegral) d8to24table
  where table = UV.fromList $! map (constructPalette palette) [0..255]
        lastElem = constructPalette palette 255 .&. 0x00FFFFFF -- 255 is transparent
        d8to24table = table UV.// [(255, lastElem)]

constructPalette :: B.ByteString -> Int -> Int
constructPalette bs i = (255 `shiftL` 24) .|. (b `shiftL` 16) .|. (g `shiftL` 8) .|. r
  where j = i * 3
        r = fromIntegral (B.index bs j) :: Int
        g = fromIntegral (B.index bs (j + 1)) :: Int
        b = fromIntegral (B.index bs (j + 2)) :: Int

loadPCX :: B.ByteString -> Bool -> Bool -> Quake (Maybe (B.ByteString, B.ByteString, (Int, Int)))
loadPCX fileName returnPalette returnDimensions =
  FS.fOpenFileWithLength fileName >>= loadPcxT >>= parseRawPCX
  where parseRawPCX Nothing = badPCXFile
        parseRawPCX (Just pcx)
          | not (validPCX pcx) = badPCXFile
          | otherwise = return (Just (decodePCX returnPalette returnDimensions pcx))
        badPCXFile =
          do VID.printf Constants.printDeveloper (B.concat ["Bad pcx file ", fileName, "\n"])
             return Nothing

validPCX :: PcxT -> Bool
validPCX pcx
  | badManufacturer || wrongVersion || invalidEncoding || invalidBPP || invalidXMax || invalidYMax = False
  | otherwise = True
  where badManufacturer = (pcx^.pcxManufacturer) /= 0x0A
        wrongVersion = (pcx^.pcxVersion) /= 5
        invalidEncoding = (pcx^.pcxEncoding) /= 1
        invalidBPP = (pcx^.pcxBitsPerPixel) /= 8
        invalidXMax = (pcx^.pcxXMax) >= 640
        invalidYMax = (pcx^.pcxYMax) >= 480

loadPcxT :: Maybe (Handle, Int) -> Quake (Maybe PcxT)
loadPcxT Nothing = return Nothing
loadPcxT (Just (fileHandle, len)) =
  do (res, _) <- request parsePcxT
     either (const (return Nothing)) (return . Just) res
  where parsePcxT = runStateT (PB.decodeGet (getPcxT len)) (PBS.fromHandle fileHandle)

decodePCX :: Bool -> Bool -> PcxT -> (B.ByteString, B.ByteString, (Int, Int))
decodePCX returnPalette returnDimensions pcx = (pix, palette, dimensions)
  where width = fromIntegral ((pcx^.pcxXMax) - (pcx^.pcxXMin) + 1)
        height = fromIntegral ((pcx^.pcxYMax) - (pcx^.pcxYMin) + 1)
        palette | returnPalette = B.drop (B.length (pcx^.pcxData) - 768) (pcx^.pcxData)
                | otherwise = B.empty
        dimensions | returnDimensions = (width, height)
                   | otherwise = (0, 0)
        pix = doDecodePCX (pcx^.pcxData) 0 0 0 width height mempty

doDecodePCX :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> BB.Builder -> B.ByteString
doDecodePCX raw idx x y maxX maxY !acc
  | y >= maxY = BL.toStrict (BB.toLazyByteString acc)
  | x >= maxX = doDecodePCX raw idx 0 (y + 1) maxX maxY acc
  | dataByte .&. 0xC0 == 0xC0
      = doDecodePCX raw (idx + 2) (x + runLength) y maxX maxY (acc <> BB.byteString (B.replicate runLength byte))
  | otherwise
      = doDecodePCX raw (idx + 1) (x + 1) y maxX maxY (acc <> BB.word8 dataByte)
  where !dataByte = B.index raw idx
        !runLength = fromIntegral (dataByte .&. 0x3F)
        !byte = B.index raw (idx + 1)

glImageListF :: XCommandT
glImageListF = error "Image.glImageListF" -- TODO

glInitImages :: Quake ()
glInitImages =
  do fastRenderAPIGlobals.frRegistrationSequence .= 1
     void (CVar.get "intensity" "2" 0)
     updateIntencity =<< intensityCVar
     setInverseIntensity =<< intensityCVar
     getPalette
     loadColorTable =<< use (fastRenderAPIGlobals.frColorTableEXT)
     gamma <- getGamma =<< use (fastRenderAPIGlobals.frGLConfig.glcRenderer)
     buildGammaTable gamma
     buildIntensityTable =<< intensityCVar
  where updateIntencity intensity
          | (intensity^.cvValue) <= 1 = void (CVar.set "intensity" "1")
          | otherwise = return ()
        setInverseIntensity intensity =
          fastRenderAPIGlobals.frGLState.glsInverseIntensity .= 1 / (intensity^.cvValue)

loadColorTable :: Bool -> Quake ()
loadColorTable False = return ()
loadColorTable True =
  do buf <- FS.loadFile "pics/16to8.dat"
     fastRenderAPIGlobals.frGLState.glsD16To8Table .= buf
     maybe colorTableError (const (return ())) buf
  where colorTableError = Com.fatalError "Couldn't load pics/16to8.pcx"

getGamma :: Int -> Quake Float
getGamma renderer
  | renderer .&. (Constants.glRendererVoodoo .|. Constants.glRendererVoodoo2) /= 0 = return 1
  | otherwise = fmap (^.cvValue) vidGammaCVar

buildGammaTable :: Float -> Quake ()
buildGammaTable gamma =
  fastRenderAPIGlobals.frGammaTable .= B.unfoldr gammaFunc 0
  where gammaFunc | gamma == 1 = simpleGamma
                  | otherwise = complexGamma gamma

simpleGamma :: Int -> Maybe (Word8, Int)
simpleGamma idx
  | idx >= 256 = Nothing
  | otherwise = Just (fromIntegral idx, idx + 1)

complexGamma :: Float -> Int -> Maybe (Word8, Int)
complexGamma gamma idx
  | idx >= 256 = Nothing
  | otherwise = Just (fromIntegral inf', idx + 1)
  where inf = truncate (255 * (((fromIntegral idx + 0.5) / 255.5) ** gamma) + 0.5) :: Int
        inf' | inf < 0 = 0
             | inf > 255 = 255
             | otherwise = inf

buildIntensityTable :: CVarT -> Quake ()
buildIntensityTable intensity =
  fastRenderAPIGlobals.frIntensityTable .= B.unfoldr (genIntensityTable v) 0
  where v = intensity^.cvValue

genIntensityTable :: Float -> Int -> Maybe (Word8, Int)
genIntensityTable v idx
  | idx >= 256 = Nothing
  | otherwise = Just (fromIntegral (min j 255), idx + 1)
  where j = truncate (fromIntegral idx * v) :: Int

glTexEnv :: GL.GLenum -> Quake ()
glTexEnv mode =
  do tmu <- use (fastRenderAPIGlobals.frGLState.glsCurrentTmu)
     lastMode <- getLastMode tmu
     when (mode /= fromIntegral lastMode) $
       do request (GL.glTexEnvi GL.GL_TEXTURE_ENV GL.GL_TEXTURE_ENV_MODE (fromIntegral mode))
          setLastMode tmu
  where getLastMode tmu
          | tmu == 0 = use (fastRenderAPIGlobals.frLastModes._1)
          | otherwise = use (fastRenderAPIGlobals.frLastModes._2)
        setLastMode tmu
          | tmu == 0 = fastRenderAPIGlobals.frLastModes._1 .= fromIntegral mode
          | otherwise = fastRenderAPIGlobals.frLastModes._2 .= fromIntegral mode

glTextureMode :: B.ByteString -> Quake ()
glTextureMode str =
  case V.find (\m -> BC.map toUpper (m^.glmName) == strUp) modes of
    Nothing ->
      VID.printf Constants.printAll (B.concat ["bad filter name: [", str, "]\n"])
    Just mode ->
      do fastRenderAPIGlobals.frGLFilterMin .= (mode^.glmMinimize)
         fastRenderAPIGlobals.frGLFilterMax .= (mode^.glmMaximize)
         numGLTextures <- use (fastRenderAPIGlobals.frNumGLTextures)
         changeMipMap mode 0 numGLTextures
  where strUp = BC.map toUpper str

changeMipMap :: GLModeT -> Int -> Int -> Quake ()
changeMipMap mode idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise =
      do image <- readRef (Ref idx)
         textureSettings image
         changeMipMap mode (idx + 1) maxIdx
  where textureSettings image
          | (image^.iType) `notElem` [Constants.itPic, Constants.itSky] =
              do glBind (image^.iTexNum)
                 request setFilters
          | otherwise = return ()
        setFilters =
          do GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral (mode^.glmMinimize))
             GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral (mode^.glmMaximize))

glTextureAlphaMode :: B.ByteString -> Quake ()
glTextureAlphaMode str =
  case V.find (\m -> BC.map toUpper (m^.gltmName) == strUp) glAlphaModes of
    Nothing -> VID.printf Constants.printAll (B.concat ["bad alpha texture mode name: [", str, "]\n"])
    Just mode -> fastRenderAPIGlobals.frGLTexAlphaFormat .= (mode^.gltmMode)
  where strUp = BC.map toUpper str

glTextureSolidMode :: B.ByteString -> Quake ()
glTextureSolidMode str =
  case V.find (\m -> BC.map toUpper (m^.gltmName) == strUp) glSolidModes of
    Nothing -> VID.printf Constants.printAll (B.concat ["bad solid texture mode name: [", str, "]\n"])
    Just mode -> fastRenderAPIGlobals.frGLTexSolidFormat .= (mode^.gltmMode)
  where strUp = BC.map toUpper str

glSetTexturePalette :: UV.Vector Int -> Quake ()
glSetTexturePalette = error "Image.glSetTexturePalette" -- TODO

glBind :: Int -> Quake ()
glBind texNum =
  do noBind <- glNoBindCVar
     drawChars <- use (fastRenderAPIGlobals.frDrawChars)
     glState <- use (fastRenderAPIGlobals.frGLState)
     bindImage glState =<< pickTexNum noBind drawChars
  where pickTexNum _ Nothing = return texNum
        pickTexNum noBind (Just drawCharsRef)
          | (noBind^.cvValue) /= 0 =
              do img <- readRef drawCharsRef
                 return (img^.iTexNum)
          | otherwise = return texNum

bindImage :: GLStateT -> Int -> Quake ()
bindImage glState texNum
  | currentTexture == texNum = return ()
  | otherwise =
      do updateCurrentTexture
         request (GL.glBindTexture GL.GL_TEXTURE_2D (fromIntegral texNum))
  where currentTexture
          | glState^.glsCurrentTmu == 0 = glState^.glsCurrentTextures._1
          | otherwise = glState^.glsCurrentTextures._2
        updateCurrentTexture
          | glState^.glsCurrentTmu == 0 = fastRenderAPIGlobals.frGLState.glsCurrentTextures._1 .= texNum
          | otherwise = fastRenderAPIGlobals.frGLState.glsCurrentTextures._2 .= texNum

glLoadPic :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> Int -> Quake (Ref ImageT)
glLoadPic name pic width height picType bits =
  do numGLTextures <- use (fastRenderAPIGlobals.frNumGLTextures)
     idx <- findFreeImage 0 numGLTextures
     updateNumGLTextures numGLTextures idx
     checkNameError
     setImageInfo idx =<< use (fastRenderAPIGlobals.frRegistrationSequence)
     image <- floodFillImage pic width height picType bits
     glBind (Constants.texNumImages + idx)
     hasAlpha <- glUploadImage image
     updateImageInfo idx hasAlpha
     return (Ref idx)
  where checkNameError
          | B.length name > Constants.maxQPath =
              Com.comError Constants.errDrop (B.concat ["Draw_LoadPic: \"", name, "\" is too long"])
          | otherwise = return ()
        setImageInfo idx rs =
          modifyRef (Ref idx) (\v -> v & iName .~ name
                                       & iRegistrationSequence .~ rs
                                       & iWidth .~ width
                                       & iHeight .~ height
                                       & iType .~ picType
                                       & iScrap .~ False
                                       & iTexNum .~ Constants.texNumImages + idx)
        glUploadImage image
          | bits == 8 = glUpload8 image width height (picType `notElem` [Constants.itPic, Constants.itSky]) (picType == Constants.itSky)
          | otherwise = glUpload32 image width height (picType `notElem` [Constants.itPic, Constants.itSky])
        updateImageInfo idx hasAlpha =
          do uploadWidth <- use (fastRenderAPIGlobals.frUploadWidth)
             uploadHeight <- use (fastRenderAPIGlobals.frUploadHeight)
             uploadedPaletted <- use (fastRenderAPIGlobals.frUploadedPaletted)
             modifyRef (Ref idx) (\v -> v & iHasAlpha .~ hasAlpha
                                          & iUploadWidth .~ uploadWidth
                                          & iUploadHeight .~ uploadHeight
                                          & iPaletted .~ uploadedPaletted
                                          & iSL .~ 0
                                          & iSH .~ 1
                                          & iTL .~ 0
                                          & iTH .~ 1)

findFreeImage :: Int -> Int -> Quake Int
findFreeImage idx numGLTextures
  | idx >= numGLTextures = return idx
  | otherwise =
      do img <- readRef (Ref idx)
         case img^.iTexNum of
           0 -> return idx
           _ -> findFreeImage (idx + 1) numGLTextures

updateNumGLTextures :: Int -> Int -> Quake ()
updateNumGLTextures numGLTextures idx
  | idx == numGLTextures =
      do checkError
         fastRenderAPIGlobals.frNumGLTextures += 1
  | otherwise = return ()
  where checkError | numGLTextures == Constants.maxGLTextures =
                       Com.comError Constants.errDrop "MAX_GLTEXTURES"
                   | otherwise = return ()

floodFillImage :: B.ByteString -> Int -> Int -> Int -> Int -> Quake B.ByteString
floodFillImage pic width height picType bits
  | picType == Constants.itSkin && bits == 8 =
      do d8to24table <- use (fastRenderAPIGlobals.frd8to24table)
         fifo <- request (use frFifo)
         request (io (rFloodFillSkin pic width height d8to24table fifo))
  | otherwise = return pic

rFloodFillSkin :: B.ByteString -> Int -> Int -> UV.Vector Int -> MV.IOVector (Int, Int) -> IO B.ByteString
rFloodFillSkin skin skinWidth skinHeight d8to24table fifo
  | fillColor == filledColor || fillColor == 255 = return skin
  | otherwise =
      do floodFill skinMutable skinWidth skinHeight fifo (fromIntegral filledColor) (fromIntegral fillColor) 1 0
         return (fromMV skinMutable)
  where fillColor = fromIntegral (B.head skin)
        filledColor = fromMaybe 0 (UV.findIndex (== 0xFF000000) d8to24table)
        skinMutable = toMV skin
        toMV (BI.PS ptr 0 n) = MSV.MVector n ptr -- ugly hack IMPROVE
        toMV _ = error "Couldn't convert ByteString to MVector" -- shouldn't happen
        fromMV (MSV.MVector n ptr) = BI.PS ptr 0 n

floodFill :: MSV.IOVector Word8 -> Int -> Int -> MV.IOVector (Int, Int) -> Word8 -> Word8 -> Int -> Int -> IO ()
floodFill skin skinWidth skinHeight fifo filledColor fillColor inpt outpt
  | inpt == outpt = return ()
  | otherwise =
      do (x, y) <- MV.read fifo outpt
         doFloodFill skin skinWidth skinHeight fifo filledColor fillColor inpt outpt x y

doFloodFill :: MSV.IOVector Word8 -> Int -> Int -> MV.IOVector (Int, Int) -> Word8 -> Word8 -> Int -> Int -> Int -> Int -> IO ()
doFloodFill skin skinWidth skinHeight fifo filledColor fillColor inpt outpt x y =
  step1 >>= step2 >>= step3 >>= step4 >>= writeAndProceed
  where pos = x + skinWidth * y
        step1
          | x > 0 = floodFillStep skin fifo inpt pos x y fillColor filledColor (-1) (-1) 0
          | otherwise = return (inpt, filledColor)
        step2 (inpt1, fdc1)
          | x < skinWidth - 1 = floodFillStep skin fifo inpt1 pos x y fillColor fdc1 1 1 0
          | otherwise = return (inpt1, fdc1) 
        step3 (inpt2, fdc2)
          | y > 0 = floodFillStep skin fifo inpt2 pos x y fillColor fdc2 (-skinWidth) 0 (-1)
          | otherwise = return (inpt2, fdc2)
        step4 (inpt3, fdc3)
          | y < skinHeight - 1 = floodFillStep skin fifo inpt3 pos x y fillColor fdc3 skinWidth 0 1
          | otherwise = return (inpt3, fdc3)
        writeAndProceed (inpt4, fdc4) =
          do MSV.write skin pos fdc4
             floodFill skin skinWidth skinHeight fifo filledColor fillColor inpt4 ((outpt + 1) .&. floodFillFifoMask)

floodFillStep :: MSV.IOVector Word8 -> MV.IOVector (Int, Int) -> Int -> Int -> Int -> Int -> Word8 -> Word8 -> Int -> Int -> Int -> IO (Int, Word8)
floodFillStep skin fifo inpt pos x y fillColor fdc off dx dy =
  fillSkin =<< MSV.read skin (pos + off)
  where fillSkin :: Word8 -> IO (Int, Word8)
        fillSkin b
          | b == fillColor =
              do MSV.write skin (pos + off) 255
                 MV.write fifo inpt (x + dx, y + dy)
                 return ((inpt + 1) .&. floodFillFifoMask, fdc)
          | b /= 255 = return (inpt, b)
          | otherwise = return (inpt, fdc)

glUpload8 :: B.ByteString -> Int -> Int -> Bool -> Bool -> Quake Bool
glUpload8 image width height mipmap isSky =
  do checkDimensions
     colorTable <- use (fastRenderAPIGlobals.frColorTableEXT)
     palettedTexture <- glExtPalettedTextureCVar
     proceedUpload colorTable palettedTexture
  where checkDimensions
          | sz > 512 * 256 =
              Com.comError Constants.errDrop "GL_Upload8: too large"
          | otherwise = return ()
        sz = width * height
        proceedUpload colorTable palettedTexture
          | colorTable && (palettedTexture^.cvValue) /= 0 && isSky =
              do request (io (BU.unsafeUseAsCString image upload))
                 filterMax <- use (fastRenderAPIGlobals.frGLFilterMax)
                 request (glApplyFilters filterMax)
                 return False
          | otherwise =
              do d8to24table <- use (fastRenderAPIGlobals.frd8to24table)
                 glUpload32 (constructTrans image width d8to24table 0 sz mempty) width height mipmap
        upload =
          GL.glTexImage2D GL.GL_TEXTURE_2D
                          0
                          (fromIntegral GL.GL_COLOR_INDEX8_EXT)
                          (fromIntegral width)
                          (fromIntegral height)
                          0
                          GL.GL_COLOR_INDEX
                          GL.GL_UNSIGNED_BYTE
        glApplyFilters filterMax =
          do GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral filterMax)
             GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral filterMax)

constructTrans :: B.ByteString -> Int -> UV.Vector Int -> Int -> Int -> BB.Builder -> B.ByteString
constructTrans image width d8to24table idx maxIdx !acc
  | idx >= maxIdx = BL.toStrict (BB.toLazyByteString acc)
  | otherwise = let !p = image `B.index` idx
                    !t = d8to24table UV.! fromIntegral p
                    !p' | p == 0xFF = scanAround
                        | otherwise = p
                    !t' | p == 0xFF = (d8to24table UV.! fromIntegral p') .&. 0x00FFFFFF
                        | otherwise = t
                    scanAround
                      | idx > width && (image `B.index` (idx - width)) /= 0xFF = image `B.index` (idx - width)
                      | idx < maxIdx - width && (image `B.index` (idx + width)) /= 0xFF = image `B.index` (idx + width)
                      | idx > 0 && (image `B.index` (idx - 1)) /= 0xFF = image `B.index` (idx - 1)
                      | idx < maxIdx - 1 && (image `B.index` (idx + 1)) /= 0xFF = image `B.index` (idx + 1)
                      | otherwise = 0
                in constructTrans image width d8to24table (idx + 1) maxIdx (acc `mappend` BB.int32LE (fromIntegral t'))

glUpload32 :: B.ByteString -> Int -> Int -> Bool -> Quake Bool
glUpload32 !image width height mipmap =
  do scaledWidth <- calcScaledValue width mipmap
     scaledHeight <- calcScaledValue height mipmap
     mapMonad (zoom fastRenderAPIGlobals) $ do
       frUploadedPaletted .= False
       frUploadWidth .= scaledWidth
       frUploadHeight .= scaledHeight
     checkScaledError scaledWidth scaledHeight
     comp <- getTextureComponents samples
     resampled <- uploadOrResample image width height mipmap scaledWidth scaledHeight samples comp
     uploadResampled resampled scaledWidth scaledHeight mipmap samples comp
     applyFilters mipmap
     return (samples == glAlphaFormat)
  where samples = scanAlpha image 0 3 (width * height)

calcScaledValue :: Int -> Bool -> Quake Int
calcScaledValue value mipmap =
  do roundDown <- glRoundDownCVar
     fmap scale (checkMipMap (checkRoundDown roundDown (calcScale 1 value)))
  where calcScale v val
          | v >= val = v
          | otherwise = calcScale (v `shiftL` 1) val
        checkRoundDown roundDown sv
          | (roundDown^.cvValue) > 0 && sv > value && mipmap = sv `shiftR` 1
          | otherwise = sv
        checkMipMap sv
          | mipmap =
              do picMip <- glPicMipCVar
                 return (sv `shiftR` truncate (picMip^.cvValue))
          | otherwise = return sv
        scale sv
          | sv > 256 = 256
          | sv < 1 = 1
          | otherwise = sv

scanAlpha :: B.ByteString -> Int -> Int -> Int -> Int
scanAlpha image idx scan maxIdx
  | idx >= maxIdx = glSolidFormat
  | image `B.index` scan /= 0xFF = glAlphaFormat
  | otherwise = scanAlpha image (idx + 1) (scan + 4) maxIdx

checkScaledError :: Int -> Int -> Quake ()
checkScaledError scaledWidth scaledHeight =
  when (scaledWidth * scaledHeight > 256 * 256) $
    Com.comError Constants.errDrop "GL_Upload32: too big"

getTextureComponents :: Int -> Quake Int
getTextureComponents samples
  | samples == glSolidFormat = use (fastRenderAPIGlobals.frGLTexSolidFormat)
  | samples == glAlphaFormat = use (fastRenderAPIGlobals.frGLTexAlphaFormat)
  | otherwise =
      do VID.printf Constants.printAll (B.concat ["Unknown number of texture components ", encode samples, "\n"])
         return samples

uploadOrResample :: B.ByteString -> Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Quake (Maybe B.ByteString)
uploadOrResample image width height mipmap scaledWidth scaledHeight samples comp
  | sameDimensions && not mipmap =
      do uploadImage image scaledWidth scaledHeight 0 samples comp
         return Nothing
  | sameDimensions = return (Just image)
  | otherwise = return (Just (glResampleTexture image width height scaledWidth scaledHeight))
  where sameDimensions = scaledWidth == width && scaledHeight == height

glResampleTexture :: B.ByteString -> Int -> Int -> Int -> Int -> B.ByteString
glResampleTexture = error "Image.glResampleTexture" -- TODO

uploadImage :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> Quake ()
uploadImage image scaledWidth scaledHeight mipLevel samples comp =
  do colorTable <- use (fastRenderAPIGlobals.frColorTableEXT)
     palettedTextureValue <- fmap (^.cvValue) glExtPalettedTextureCVar
     proceedUploadImage colorTable palettedTextureValue
  where proceedUploadImage colorTable palettedTextureValue
          | colorTable && palettedTextureValue /= 0 && samples == glSolidFormat =
              do fastRenderAPIGlobals.frUploadedPaletted .= True
                 palettedTexture <- glBuildPalettedTexture image scaledWidth scaledHeight
                 request (io (BU.unsafeUseAsCString palettedTexture (upload GL.GL_COLOR_INDEX8_EXT GL.GL_COLOR_INDEX)))
          | otherwise =
              request (io (BU.unsafeUseAsCString image (upload comp GL.GL_RGBA)))
        upload internalFormat format =
          GL.glTexImage2D GL.GL_TEXTURE_2D
                          (fromIntegral mipLevel) 
                          (fromIntegral internalFormat)
                          (fromIntegral scaledWidth)
                          (fromIntegral scaledHeight)
                          0
                          format
                          GL.GL_UNSIGNED_BYTE

uploadResampled :: Maybe B.ByteString -> Int -> Int -> Bool -> Int -> Int -> Quake ()
uploadResampled Nothing _ _ _ _ _ = return ()
uploadResampled (Just image) scaledWidth scaledHeight mipmap samples comp =
  do scaled <- glLightScaleTexture image scaledWidth scaledHeight (not mipmap)
     uploadImage scaled scaledWidth scaledHeight 0 samples comp
     when mipmap $
       uploadMipMaps scaled scaledWidth scaledHeight 0 samples comp

glLightScaleTexture :: B.ByteString -> Int -> Int -> Bool -> Quake B.ByteString
glLightScaleTexture image width height onlyGamma =
  do gammaTable <- use (fastRenderAPIGlobals.frGammaTable)
     lightscale gammaTable
  where lightscale gammaTable
          | onlyGamma = return (buildFromGammaTable image gammaTable 0 0 c mempty)
          | otherwise =
              do intensityTable <- use (fastRenderAPIGlobals.frIntensityTable)
                 return (buildFromGammaAndIntensityTable image gammaTable intensityTable 0 0 c mempty)
        c = width * height

buildFromGammaTable :: B.ByteString -> B.ByteString -> Int -> Int -> Int -> BB.Builder -> B.ByteString
buildFromGammaTable image gammaTable idx p maxIdx acc
  | idx >= maxIdx = BL.toStrict (BB.toLazyByteString acc)
  | otherwise = buildFromGammaTable image gammaTable (idx + 1) (p + 4) maxIdx (mconcat [acc, BB.word8 a, BB.word8 b, BB.word8 c, BB.word8 p3])
      where p3 = image `B.index` (p + 3)
            a = gammaTable `B.index` fromIntegral (image `B.index` p)
            b = gammaTable `B.index` fromIntegral (image `B.index` (p + 1))
            c = gammaTable `B.index` fromIntegral (image `B.index` (p + 2))
  
buildFromGammaAndIntensityTable :: B.ByteString -> B.ByteString -> B.ByteString -> Int -> Int -> Int -> BB.Builder -> B.ByteString
buildFromGammaAndIntensityTable image gammaTable intensityTable idx p maxIdx acc
  | idx >= maxIdx = BL.toStrict (BB.toLazyByteString acc)
  | otherwise = buildFromGammaAndIntensityTable image gammaTable intensityTable (idx + 1) (p + 4) maxIdx (mconcat [acc, BB.word8 a, BB.word8 b, BB.word8 c, BB.word8 p3])
      where p3 = image `B.index` (p + 3)
            i0 = intensityTable `B.index` fromIntegral (image `B.index` p)
            i1 = intensityTable `B.index` fromIntegral (image `B.index` (p + 1))
            i2 = intensityTable `B.index` fromIntegral (image `B.index` (p + 2))
            a = gammaTable `B.index` fromIntegral i0
            b = gammaTable `B.index` fromIntegral i1
            c = gammaTable `B.index` fromIntegral i2

uploadMipMaps :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> Quake ()
uploadMipMaps image scaledWidth scaledHeight mipLevel samples comp
  | scaledWidth <= 1 && scaledHeight <= 1 = return ()
  | otherwise =
      do uploadImage scaled scaledWidth' scaledHeight' mipLevel' samples comp
         uploadMipMaps scaled scaledWidth' scaledHeight' mipLevel' samples comp
  where scaled = glMipMap image scaledWidth scaledHeight
        sw = scaledWidth `shiftR` 1
        sh = scaledHeight `shiftR` 1
        scaledWidth' = max 1 sw
        scaledHeight' = max 1 sh
        mipLevel' = mipLevel + 1

glMipMap :: B.ByteString -> Int -> Int -> B.ByteString
glMipMap image width height
  | width == 1 || height == 1 = B.replicate (width * height) 255 -- TODO: UGLY HACK!!!!!! FIXME!! mipmap generation fails when w or h is 1 (in jake2 and quake2 it uses the fact that input array is huge and can overflow, but we cannot)
  | otherwise = forI 0 0 (height `shiftR` 1) mempty
  where forI inIdx i maxI acc
          | i >= maxI = BL.toStrict (BB.toLazyByteString acc)
          | otherwise =
              let w = width `shiftL` 2
                  (inIdx', r) = forJ inIdx 0 w mempty
              in forI (inIdx' + w) (i + 1) maxI (acc `mappend` r)
        forJ inIdx j maxJ acc
          | j >= maxJ = (inIdx, acc)
          | otherwise =
              let w = width `shiftL` 2
                  a = ((image `B.index` (inIdx + 0)) + (image `B.index` (inIdx + 4)) + (image `B.index` (inIdx + w + 0)) + (image `B.index` (inIdx + w + 4))) `shiftR` 2
                  b = ((image `B.index` (inIdx + 1)) + (image `B.index` (inIdx + 5)) + (image `B.index` (inIdx + w + 1)) + (image `B.index` (inIdx + w + 5))) `shiftR` 2
                  c = ((image `B.index` (inIdx + 2)) + (image `B.index` (inIdx + 6)) + (image `B.index` (inIdx + w + 2)) + (image `B.index` (inIdx + w + 6))) `shiftR` 2
                  d = ((image `B.index` (inIdx + 3)) + (image `B.index` (inIdx + 7)) + (image `B.index` (inIdx + w + 3)) + (image `B.index` (inIdx + w + 7))) `shiftR` 2
              in forJ (inIdx + 8) (j + 8) maxJ (mconcat [acc, BB.word8 a, BB.word8 b, BB.word8 c, BB.word8 d])

applyFilters :: Bool -> Quake ()
applyFilters mipmap =
  do filterMin <- use (fastRenderAPIGlobals.frGLFilterMin)
     filterMax <- use (fastRenderAPIGlobals.frGLFilterMax)
     request (proceedApplyFilters filterMin filterMax)
  where proceedApplyFilters filterMin filterMax
          | mipmap = glApplyFilters filterMin filterMax
          | otherwise = glApplyFilters filterMax filterMax
        glApplyFilters a b =
              do GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral a)
                 GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral b)

glBuildPalettedTexture :: B.ByteString -> Int -> Int -> Quake B.ByteString
glBuildPalettedTexture = error "Image.glBuildPalettedTexture" -- TODO

glFindImage :: B.ByteString -> Int -> Quake (Maybe (Ref ImageT))
glFindImage imageName imageType
  | B.length imageName < 1 = return Nothing
  | otherwise = do
      numGLTextures <- use (fastRenderAPIGlobals.frNumGLTextures)
      imageRef <- findImage imageName 0 numGLTextures
      maybe (loadFromDisk imageName imageType) updateAndReturn imageRef

loadFromDisk :: B.ByteString -> Int -> Quake (Maybe (Ref ImageT))
loadFromDisk imageName imageType
  | ".pcx" `BC.isSuffixOf` imageName =
      do imageData <- loadPCX imageName False True
         maybe (return Nothing) loadPCXImage imageData
  | ".wal" `BC.isSuffixOf` imageName = fmap Just (glLoadWal imageName)
  | ".tga" `BC.isSuffixOf` imageName =
      do imageData <- loadTGA imageName
         maybe (return Nothing) loadTGAImage imageData
  | otherwise =
      do imageData <- loadPCX (B.concat ["pics/", imageName, ".pcx"]) False True
         maybe (return Nothing) loadPCXImage imageData
  where loadPCXImage (pic, _, (width, height)) =
          fmap Just (glLoadPic imageName pic width height imageType 8)
        loadTGAImage (pic, (width, height)) =
          fmap Just (glLoadPic imageName pic width height imageType 32)

updateAndReturn :: Ref ImageT -> Quake (Maybe (Ref ImageT))
updateAndReturn imageRef =
  do rs <- use (fastRenderAPIGlobals.frRegistrationSequence)
     modifyRef imageRef (\v -> v & iRegistrationSequence .~ rs)
     return (Just imageRef)

findImage :: B.ByteString -> Int -> Int -> Quake (Maybe (Ref ImageT))
findImage imageName idx maxIdx
  | idx >= maxIdx = return Nothing
  | otherwise =
      do image <- readRef imageRef
         checkAndProceed (image^.iName)
  where imageRef = Ref idx
        checkAndProceed name
          | imageName == name = return (Just imageRef)
          | otherwise = findImage imageName (idx + 1) maxIdx

glLoadWal :: B.ByteString -> Quake (Ref ImageT)
glLoadWal name = FS.fOpenFile name >>= loadMiptexT >>= uploadMiptexT name

loadMiptexT :: Maybe Handle -> Quake (Maybe MiptexT)
loadMiptexT Nothing = return Nothing
loadMiptexT (Just fileHandle) =
  do (res, _) <- request parseMiptexT
     either (const (return Nothing)) (return . Just) res
  where parseMiptexT = runStateT (PB.decodeGet getMiptexT) (PBS.fromHandle fileHandle)

uploadMiptexT :: B.ByteString -> Maybe MiptexT -> Quake (Ref ImageT)
uploadMiptexT name Nothing =
  do VID.printf Constants.printAll (B.concat ["GL_FindImage: can't load ", name, "\n"])
     use (fastRenderAPIGlobals.frNoTexture)
uploadMiptexT name (Just miptex) =
  glLoadPic name (miptex^.mtBuf) (miptex^.mtWidth) (miptex^.mtHeight) Constants.itWall 8

loadTGA :: B.ByteString -> Quake (Maybe (B.ByteString, (Int, Int)))
loadTGA = error "Image.loadTGA" -- TODO
