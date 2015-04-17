{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Image where

import Control.Lens ((^.), (.=))
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.QFiles.PcxT
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.FS as FS

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
