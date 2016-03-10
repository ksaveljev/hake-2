module Render.Fast.Image
  ( getPalette
  , glImageListF
  , glInitImages
  , glShutdownImages
  , rRegisterSkin
  ) where

import {-# SOURCE #-} qualified Client.VID as VID
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.FS as FS
import           QCommon.QFiles.PcxT
import           QuakeIOState
import           QuakeState
import           Render.ImageT
import           Types

import           Control.Lens (use, (^.), (.=))
import           Control.Monad.State.Strict (runStateT)
import           Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.IORef (IORef)
import           Data.Monoid ((<>))
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import           Foreign.Marshal.Utils (with)
import qualified Graphics.GL as GL
import qualified Pipes.Binary as PB
import qualified Pipes.ByteString as PBS
import           System.IO (Handle)

rRegisterSkin :: B.ByteString -> Quake (Maybe (IORef ImageT))
rRegisterSkin = error "Image.rRegisterSkin" -- TODO

glShutdownImages :: Quake ()
glShutdownImages =
  do numTextures <- use (fastRenderAPIGlobals.frNumGLTextures)
     request $
       do textures <- use frGLTextures
          clearTextures textures 0 numTextures

clearTextures :: MV.IOVector ImageT -> Int -> Int -> QuakeIO ()
clearTextures textures idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise =
      do image <- MV.read textures idx
         clearWithRegSeq (image^.iTexNum) (image^.iRegistrationSequence)
         clearTextures textures (idx + 1) maxIdx
  where clearWithRegSeq _ 0 = return ()
        clearWithRegSeq texNum _ = io $
          with (fromIntegral texNum) $ \ptr ->
            do GL.glDeleteTextures 1 ptr
               MV.write textures idx (newImageT idx)

getPalette :: Quake ()
getPalette =
  do (_, result, _) <- loadPCX "pics/colormap.pcx" True False
     proceedConstructPalette result
  where proceedConstructPalette Nothing = paletteError
        proceedConstructPalette (Just palette)
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

loadPCX :: B.ByteString -> Bool -> Bool -> Quake (Maybe B.ByteString, Maybe B.ByteString, Maybe (Int, Int))
loadPCX fileName returnPalette returnDimensions =
  FS.fOpenFileWithLength fileName >>= loadPcxT >>= parseRawPCX
  where parseRawPCX Nothing = badPCXFile
        parseRawPCX (Just pcx)
          | not (validPCX pcx) = badPCXFile
          | otherwise = return (decodePCX returnPalette returnDimensions pcx)
        badPCXFile =
          do VID.printf Constants.printDeveloper (B.concat ["Bad pcx file ", fileName, "\n"])
             return (Nothing, Nothing, Nothing)

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

decodePCX :: Bool -> Bool -> PcxT -> (Maybe B.ByteString, Maybe B.ByteString, Maybe (Int, Int))
decodePCX returnPalette returnDimensions pcx = (Just pix, palette, dimensions)
  where width = fromIntegral ((pcx^.pcxXMax) - (pcx^.pcxXMin) + 1)
        height = fromIntegral ((pcx^.pcxYMax) - (pcx^.pcxYMin) + 1)
        palette | returnPalette = Just (B.drop (B.length (pcx^.pcxData) - 768) (pcx^.pcxData))
                | otherwise = Nothing
        dimensions | returnDimensions = Just (width, height)
                   | otherwise = Nothing
        pix = doDecodePCX (pcx^.pcxData) 0 0 0 width height mempty

doDecodePCX :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> BB.Builder -> B.ByteString
doDecodePCX raw idx x y maxX maxY acc
  | y >= maxY = BL.toStrict (BB.toLazyByteString acc)
  | x >= maxX = doDecodePCX raw idx 0 (y + 1) maxX maxY acc
  | dataByte .&. 0xC0 == 0xC0
      = doDecodePCX raw (idx + 2) (x + runLength) y maxX maxY (buildAcc acc (BB.word8 byte) runLength)
  | otherwise
      = doDecodePCX raw (idx + 1) (x + 1) y maxX maxY (acc <> BB.word8 dataByte)
  where dataByte = B.index raw idx
        runLength = fromIntegral (dataByte .&. 0x3F)
        byte = B.index raw (idx + 1)

buildAcc :: BB.Builder -> BB.Builder -> Int -> BB.Builder
buildAcc acc byte idx
  | idx <= 0 = acc
  | otherwise = buildAcc (acc <> byte) byte (idx - 1)

glImageListF :: XCommandT
glImageListF = error "Image.glImageListF" -- TODO

glInitImages :: Quake ()
glInitImages = error "Image.glInitImages" -- TODO
