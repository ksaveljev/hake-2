{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Image where

import Control.Lens ((^.), (.=), use, preuse, ix, _1, _2)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char (toUpper)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.QFiles.PcxT
import QCommon.XCommandT
import Render.GLModeT
import Render.GLTModeT
import qualified Constants
import qualified Client.VID as VID
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.RenderAPIConstants as RenderAPIConstants

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
glInitImages = io (putStrLn "Image.glInitImages") >> undefined -- TODO

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
glBind _ = io (putStrLn "Image.glBind") >> undefined -- TODO
