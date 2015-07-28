{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Light where

import Control.Lens ((^.), preuse, use, (.=), ix, _1, _2, (%=))
import Control.Monad (when, liftM, unless)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (fromJust, isNothing)
import Data.Monoid ((<>), mempty)
import Data.Word (Word8)
import Linear (V3, dot, _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified QCommon.Com as Com

dLightCutoff :: Float
dLightCutoff = 64

rPushDLights :: Quake ()
rPushDLights = do
    flashBlendValue <- liftM (^.cvValue) glFlashBlendCVar

    when (flashBlendValue == 0) $ do
      frameCount <- use $ fastRenderAPIGlobals.frFrameCount
      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
      Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      worldModel <- io $ readIORef worldModelRef

      fastRenderAPIGlobals.frDLightFrameCount .= frameCount + 1 -- because the count hasn't advanced yet for this frame

      markLights newRefDef worldModel 0 (newRefDef^.rdNumDLights)

  where markLights :: RefDefT -> ModelT -> Int -> Int -> Quake ()
        markLights newRefDef worldModel idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let light = (newRefDef^.rdDLights) V.! idx
              rMarkLights light (1 `shiftL` idx) (MNodeChildReference $ (worldModel^.mNodes) V.! 0)
              markLights newRefDef worldModel (idx + 1) maxIdx

rMarkLights :: DLightT -> Int -> MNodeChild -> Quake ()
rMarkLights _ _ (MLeafChildReference _) = return ()
rMarkLights light bit (MNodeChildReference nodeRef) = do
    node <- io $ readIORef nodeRef
    splitPlane <- io $ readIORef (node^.mnPlane)

    let dist = (light^.dlOrigin) `dot` (splitPlane^.cpNormal) - (splitPlane^.cpDist)

    if | dist > (light^.dlIntensity) - dLightCutoff -> rMarkLights light bit (node^.mnChildren._1)
       | dist < dLightCutoff - (light^.dlIntensity) -> rMarkLights light bit (node^.mnChildren._2)
       | otherwise -> do
           Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
           worldModel <- io $ readIORef worldModelRef

           markPolygons worldModel node 0 (node^.mnNumSurfaces)

           rMarkLights light bit (node^.mnChildren._1)
           rMarkLights light bit (node^.mnChildren._2)

  where markPolygons :: ModelT -> MNodeT -> Int -> Int -> Quake ()
        markPolygons worldModel node idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let surfRef = (worldModel^.mSurfaces) V.! ((node^.mnFirstSurface) + idx)
              surf <- io $ readIORef surfRef
              plane <- io $ readIORef (fromJust $ surf^.msPlane)

              let dist = (light^.dlOrigin) `dot` (plane^.cpNormal) - (plane^.cpDist)
                  sidebit = if dist >= 0 then 0 else Constants.surfPlaneBack

              if (surf^.msFlags) .&. Constants.surfPlaneBack /= sidebit
                then markPolygons worldModel node (idx + 1) maxIdx
                else do
                  dLightFrameCount <- use $ fastRenderAPIGlobals.frDLightFrameCount

                  when ((surf^.msDLightFrame) /= dLightFrameCount) $
                    io $ modifyIORef' surfRef (\v -> v { _msDLightBits = 0
                                                       , _msDLightFrame = dLightFrameCount
                                                       })

                  io $ modifyIORef' surfRef (\v -> v { _msDLightBits = (v^.msDLightBits) .|. bit })

                  markPolygons worldModel node (idx + 1) maxIdx

rRenderDLights :: Quake ()
rRenderDLights = do
    io (putStrLn "Light.rRenderDLights") >> undefined -- TODO

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint _ = do
    io (putStrLn "Light.rLightPoint") >> undefined -- TODO

rSetCacheState :: IORef MSurfaceT -> Quake ()
rSetCacheState surfRef = do
    surf <- io $ readIORef surfRef
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    let updates = updateCachedLight surf newRefDef 0 Constants.maxLightMaps []
    io $ modifyIORef' surfRef (\v -> v { _msCachedLight = (surf^.msCachedLight) UV.// updates })

  where updateCachedLight :: MSurfaceT -> RefDefT -> Int -> Int -> [(Int, Float)] -> [(Int, Float)]
        updateCachedLight surf newRefDef idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let f = (surf^.msStyles) `B.index` idx
              in if f == 255
                   then acc
                   else let w = ((newRefDef^.rdLightStyles) V.! (fromIntegral f))^.lsWhite
                        in updateCachedLight surf newRefDef (idx + 1) maxIdx ((idx, w) : acc)

{-
- R_BuildLightMap
- 
- Combine and scale multiple lightmaps into the floating format in blocklights
-}
rBuildLightMap :: MSurfaceT -> Int -> Quake B.ByteString
rBuildLightMap surf stride = do
    when ((surf^.msTexInfo.mtiFlags) .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) /= 0) $
      Com.comError Constants.errDrop "R_BuildLightMap called for non-lit surface"

    let smax = fromIntegral $ ((surf^.msExtents._1) `shiftR` 4) + 1
        tmax = fromIntegral $ ((surf^.msExtents._2) `shiftR` 4) + 1
        size = smax * tmax

    blockLights <- use $ fastRenderAPIGlobals.frBlockLights
    when (size > ((UV.length blockLights) * Constants.sizeOfFloat) `shiftR` 4) $
      Com.comError Constants.errDrop "Bad s_blocklights size"

    -- set to full bright if no light data
    gotoStore <- if isNothing (surf^.msSamples)
                   then do
                     let updates = zip [0..size * 3 - 1] (repeat 255)
                     fastRenderAPIGlobals.frBlockLights %= (UV.// updates)
                     return True
                   else
                     return False

    unless gotoStore $ do
      -- count the # of maps
      let numMaps = case B.findIndex (== 255) (surf^.msStyles) of
                      Nothing -> B.length (surf^.msStyles)
                      Just idx -> idx
          lightmap = fromJust (surf^.msSamples)

      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
      glModulateValue <- liftM (^.cvValue) glModulateCVar
      blockLights' <- use $ fastRenderAPIGlobals.frBlockLights

      if numMaps == 1
        then do
          let blockLights'' = addLightMaps blockLights' lightmap 0 newRefDef glModulateValue size 0 Constants.maxLightMaps
          fastRenderAPIGlobals.frBlockLights .= blockLights''
        else do
          let blockLights'' = updateLightMaps (blockLights' UV.// (zip [0..size * 3 - 1] (repeat 0))) lightmap 0 newRefDef glModulateValue size 0 Constants.maxLightMaps
          fastRenderAPIGlobals.frBlockLights .= blockLights''

      -- add all the dynamic lights
      frameCount <- use $ fastRenderAPIGlobals.frFrameCount
      when ((surf^.msDLightFrame) == frameCount) $
        rAddDynamicLights surf

    -- put into texture format
    let stride' = stride - (smax `shiftL` 2)
    monoLightMap <- liftM (^.cvString) glMonoLightMapCVar
    blockLights' <- use $ fastRenderAPIGlobals.frBlockLights

    return $ if monoLightMap `BC.index` 0 == '0'
               then buildLightMap blockLights' 0 stride' 0 tmax 0 smax mempty
               else buildLightMapAlpha monoLightMap blockLights' 0 stride' 0 tmax 0 smax mempty

  where addLightMaps :: UV.Vector Float -> B.ByteString -> Int -> RefDefT -> Float -> Int -> Int -> Int -> UV.Vector Float
        addLightMaps blockLights lightmap lightmapIndex newRefDef glModulateValue size idx maxIdx
          | idx >= maxIdx = blockLights
          | (surf^.msStyles) `B.index` idx == 255 = blockLights
          | otherwise =
              let rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral $ (surf^.msStyles) `B.index` idx))^.lsRGB
                  scale0 = glModulateValue * (rgb^._x)
                  scale1 = glModulateValue * (rgb^._y)
                  scale2 = glModulateValue * (rgb^._z)
                  (lightmapIndex', updates) = if all (== 1) [scale0, scale1, scale2]
                                                then setLightmap lightmap lightmapIndex 0 size []
                                                else setLightmapScale lightmap lightmapIndex scale0 scale1 scale2 0 size []
              in addLightMaps (blockLights UV.// updates) lightmap lightmapIndex' newRefDef glModulateValue size (idx + 1) maxIdx

        setLightmap :: B.ByteString -> Int -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
        setLightmap lightmap lightmapIndex idx maxIdx acc
          | idx >= maxIdx = (lightmapIndex, acc)
          | otherwise =
              let a = fromIntegral $ lightmap `B.index` (lightmapIndex + 0)
                  b = fromIntegral $ lightmap `B.index` (lightmapIndex + 1)
                  c = fromIntegral $ lightmap `B.index` (lightmapIndex + 2)
              in setLightmap lightmap (lightmapIndex + 3) (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

        setLightmapScale :: B.ByteString -> Int -> Float -> Float -> Float -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
        setLightmapScale lightmap lightmapIndex scale0 scale1 scale2 idx maxIdx acc
          | idx >= maxIdx = (lightmapIndex, acc)
          | otherwise =
              let a = scale0 * (fromIntegral $ lightmap `B.index` (lightmapIndex + 0))
                  b = scale1 * (fromIntegral $ lightmap `B.index` (lightmapIndex + 1))
                  c = scale2 * (fromIntegral $ lightmap `B.index` (lightmapIndex + 2))
              in setLightmapScale lightmap (lightmapIndex + 3) scale0 scale1 scale2 (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

        updateLightMaps :: UV.Vector Float -> B.ByteString -> Int -> RefDefT -> Float -> Int -> Int -> Int -> UV.Vector Float
        updateLightMaps blockLights lightmap lightmapIndex newRefDef glModulateValue size idx maxIdx
          | idx >= maxIdx = blockLights
          | (surf^.msStyles) `B.index` idx == 255 = blockLights
          | otherwise =
              let rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral $ (surf^.msStyles) `B.index` idx))^.lsRGB
                  scale0 = glModulateValue * (rgb^._x)
                  scale1 = glModulateValue * (rgb^._y)
                  scale2 = glModulateValue * (rgb^._z)
                  (lightmapIndex', updates) = if all (== 1) [scale0, scale1, scale2]
                                                then updateLightmap blockLights lightmap lightmapIndex 0 size []
                                                else updateLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 0 size []
              in updateLightMaps (blockLights UV.// updates) lightmap lightmapIndex' newRefDef glModulateValue size (idx + 1) maxIdx

        updateLightmap :: UV.Vector Float -> B.ByteString -> Int -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
        updateLightmap blockLights lightmap lightmapIndex idx maxIdx acc
          | idx >= maxIdx = (lightmapIndex, acc)
          | otherwise =
              let a = (blockLights UV.! (idx * 3 + 0)) + (fromIntegral $ lightmap `B.index` (lightmapIndex + 0))
                  b = (blockLights UV.! (idx * 3 + 1)) + (fromIntegral $ lightmap `B.index` (lightmapIndex + 1))
                  c = (blockLights UV.! (idx * 3 + 2)) + (fromIntegral $ lightmap `B.index` (lightmapIndex + 2))
              in updateLightmap blockLights lightmap (lightmapIndex + 3) (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

        updateLightmapScale :: UV.Vector Float -> B.ByteString -> Int -> Float -> Float -> Float -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
        updateLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 idx maxIdx acc
          | idx >= maxIdx = (lightmapIndex, acc)
          | otherwise =
              let a = (blockLights UV.! (idx * 3 + 0)) + scale0 * (fromIntegral $ lightmap `B.index` (lightmapIndex + 0))
                  b = (blockLights UV.! (idx * 3 + 1)) + scale1 * (fromIntegral $ lightmap `B.index` (lightmapIndex + 1))
                  c = (blockLights UV.! (idx * 3 + 2)) + scale2 * (fromIntegral $ lightmap `B.index` (lightmapIndex + 2))
              in updateLightmapScale blockLights lightmap (lightmapIndex + 3) scale0 scale1 scale2 (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

        buildLightMap :: UV.Vector Float -> Int -> Int -> Int -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        buildLightMap blockLights blp stride i tmax j smax builder
          | i >= tmax = BL.toStrict (BB.toLazyByteString builder)
          | j >= smax = buildLightMap blockLights blp stride (i + 1) tmax 0 smax (fillStride builder 0 stride)
          | otherwise =
              let r = (truncate $ blockLights UV.! blp) :: Int
                  g = (truncate $ blockLights UV.! blp) :: Int
                  b = (truncate $ blockLights UV.! blp) :: Int
                  -- catch negative lights
                  r' = if r < 0 then 0 else r
                  g' = if g < 0 then 0 else g
                  b' = if b < 0 then 0 else b
                  -- determine the brightest of the three color components
                  brightest = maximum [r', g', b']
                  -- alpha is ONLY used for the mono lightmap case. For
                  -- this reason we set it to the brightest of the color
                  -- components so that thigs don't get too dim
                  a = brightest
                  -- rescale all the color components if the intensity of
                  -- the greatest channel exceeds 1.0
                  t = (255 / (fromIntegral brightest)) :: Float
                  r'' = if brightest > 255 then truncate (fromIntegral r' * t) else fromIntegral r'
                  g'' = if brightest > 255 then truncate (fromIntegral g' * t) else fromIntegral g'
                  b'' = if brightest > 255 then truncate (fromIntegral b' * t) else fromIntegral g'
                  a' = if brightest > 255 then truncate (fromIntegral a * t) else fromIntegral a
              in buildLightMap blockLights (blp + 3) stride i tmax (j + 1) smax (builder <> BB.word8 r'' <> BB.word8 g'' <> BB.word8 b'' <> BB.word8 a')

        buildLightMapAlpha :: B.ByteString -> UV.Vector Float -> Int -> Int -> Int -> Int -> Int -> Int -> BB.Builder -> B.ByteString
        buildLightMapAlpha monoLightMap blockLights blp stride i tmax j smax builder
          | i >= tmax = BL.toStrict (BB.toLazyByteString builder)
          | j >= smax = buildLightMap blockLights blp stride (i + 1) tmax 0 smax (fillStride builder 0 stride)
          | otherwise =
              let r = (truncate $ blockLights UV.! blp) :: Int
                  g = (truncate $ blockLights UV.! blp) :: Int
                  b = (truncate $ blockLights UV.! blp) :: Int
                  -- catch negative lights
                  r' = if r < 0 then 0 else r
                  g' = if g < 0 then 0 else g
                  b' = if b < 0 then 0 else b
                  -- determine the brightest of the three color components
                  brightest = maximum [r', g', b']
                  -- alpha is ONLY used for the mono lightmap case. For
                  -- this reason we set it to the brightest of the color
                  -- components so that thigs don't get too dim
                  a = brightest
                  -- rescale all the color components if the intensity of
                  -- the greatest channel exceeds 1.0
                  t = (255 / (fromIntegral brightest)) :: Float
                  r'' = if brightest > 255 then truncate (fromIntegral r' * t) else r'
                  g'' = if brightest > 255 then truncate (fromIntegral g' * t) else g'
                  b'' = if brightest > 255 then truncate (fromIntegral b' * t) else g'
                  a' = if brightest > 255 then truncate (fromIntegral a * t) else a
                  -- So if we are doing alpha lightmaps we need to set the
                  -- R, G, and B components to 0 and we need to set alpha to 1-alpha
                  (r''', g''', b''', a'') = updateRGBA monoLightMap r'' g'' b'' a'
              in buildLightMapAlpha monoLightMap blockLights (blp + 3) stride i tmax (j + 1) smax (builder <> BB.word8 r''' <> BB.word8 g''' <> BB.word8 b''' <> BB.word8 a'')

        fillStride :: BB.Builder -> Int -> Int -> BB.Builder
        fillStride builder idx maxIdx
          | idx >= maxIdx = builder
          | otherwise = fillStride (builder <> (BB.word8 0)) (idx + 1) maxIdx

        updateRGBA :: B.ByteString -> Int -> Int -> Int -> Int -> (Word8, Word8, Word8, Word8)
        updateRGBA monoLightMap r g b a =
          let c = monoLightMap `BC.index` 0
          in if | c == 'L' || c == 'I' -> (fromIntegral a, 0, 0, fromIntegral a)
                | c == 'C' ->
                    let a' = 255 - ((fromIntegral $ r + g + b) / 3)
                        af = a' / 255
                        r' = fromIntegral r * af
                        g' = fromIntegral g * af
                        b' = fromIntegral b * af
                    in (truncate r', truncate g', truncate b', truncate a')
                | otherwise -> (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral (255 - a))

rAddDynamicLights :: MSurfaceT -> Quake ()
rAddDynamicLights _ = do
    io (putStrLn "Light.rAddDynamicLights") >> undefined -- TODO
