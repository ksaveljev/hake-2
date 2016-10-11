{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Light
    ( rBuildLightMap
    , rLightPoint
    , rPushDLights
    , rRenderDLights
    , rSetCacheState
    ) where

import           Control.Lens                 (use, (^.), (.=), (%=), _1, _2)
import           Control.Monad                (void, when, unless)
import           Control.Monad.ST             (ST, runST)
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Maybe                   (fromMaybe)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV
import           Data.Word                    (Word8)
import           Linear                       (V3, _x, _y, _z)

import           Client.LightStyleT
import           Client.RefDefT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com                  as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Types

rPushDLights :: Quake ()
rPushDLights = error "Light.rPushDLights" -- TODO

rRenderDLights :: Quake ()
rRenderDLights = error "Light.rRenderDLights" -- TODO

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint = error "Light.rLightPoint" -- TODO

rSetCacheState :: Ref MSurfaceT -> Quake ()
rSetCacheState surfRef = do
    surf <- readRef surfRef
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    return $ runST $ do
        lights <- UV.unsafeThaw (surf^.msCachedLight)
        cacheLights newRefDef surf lights 0 Constants.maxLightMaps
        void (UV.unsafeFreeze lights)

cacheLights :: RefDefT -> MSurfaceT -> UV.MVector s Float -> Int -> Int -> ST s ()
cacheLights newRefDef surf lights idx maxIdx
    | idx >= maxIdx = return ()
    | style == 255 = return ()
    | otherwise = do
        MUV.write lights idx (((newRefDef^.rdLightStyles) V.! (fromIntegral style))^.lsWhite)
        cacheLights newRefDef surf lights (idx + 1) maxIdx
  where
    style = (surf^.msStyles) `B.index` idx

rBuildLightMap :: Ref MSurfaceT -> Maybe B.ByteString -> SV.Vector Word8 -> Int -> Int -> Quake ()
rBuildLightMap surfRef lightData buffer offset stride = do
    surf <- readRef surfRef
    blockLights <- use (fastRenderAPIGlobals.frBlockLights)
    buildLightMap surf lightData blockLights buffer offset stride

buildLightMap :: MSurfaceT -> Maybe B.ByteString -> UV.Vector Float -> SV.Vector Word8 -> Int -> Int -> Quake ()
buildLightMap surf lightData blockLights buffer offset stride = do
    checkForErrors
    goToStore <- checkLightData lightData
    unless goToStore $
        addUpdateLightMaps surf lightData size
    putIntoTextureFormat buffer offset stride tmax smax
  where
    smax = fromIntegral ((surf^.msExtents._1) `shiftR` 4) + 1
    tmax = fromIntegral ((surf^.msExtents._2) `shiftR` 4) + 1
    size = smax * tmax
    checkForErrors = do
        texInfo <- readRef (surf^.msTexInfo)
        when ((texInfo^.mtiFlags) .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) /= 0) $
            Com.comError Constants.errDrop "R_BuildLightMap called for non-lit surface"
        when (size > ((UV.length blockLights) * Constants.sizeOfFloat) `shiftR` 4) $
            Com.comError Constants.errDrop "Bad s_blocklights size"
    checkLightData Nothing = do
        fastRenderAPIGlobals.frBlockLights %= (UV.// (zip [0..size * 3 - 1] (repeat 255)))
        return True
    checkLightData _ = return False

addUpdateLightMaps :: MSurfaceT -> Maybe B.ByteString -> Int -> Quake ()
addUpdateLightMaps surf lightData size = do
    lightmap <- getLightmap (surf^.msSamples) lightData
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    glModulate <- fmap (^.cvValue) glModulateCVar
    blockLights <- use (fastRenderAPIGlobals.frBlockLights)
    fastRenderAPIGlobals.frBlockLights .= doAddUpdateLightMaps blockLights lightmap newRefDef glModulate
    frameCount <- use (fastRenderAPIGlobals.frFrameCount)
    when ((surf^.msDLightFrame) == frameCount) $
        rAddDynamicLights surf
  where
    numMaps = fromMaybe (B.length (surf^.msStyles)) (B.findIndex (== 255) (surf^.msStyles))
    getLightmap Nothing _ = do
        Com.fatalError "Light.addUpdateLightMaps surf^.msSamples cannot be Nothing"
        return B.empty
    getLightmap _ Nothing = do
        Com.fatalError "Light.addUpdateLightMaps lightData cannot be Nothing"
        return B.empty
    getLightmap (Just offset) (Just lightmap) = return (B.drop offset lightmap)
    doAddUpdateLightMaps blockLights lightmap newRefDef glModulate
        | numMaps == 1 =
            addLightMaps surf blockLights lightmap 0 newRefDef glModulate size 0 Constants.maxLightMaps
        | otherwise =
            updateLightMaps surf (blockLights UV.// (zip [0..size * 3 - 1] (repeat 0))) lightmap 0 newRefDef glModulate size 0 Constants.maxLightMaps

addLightMaps :: MSurfaceT -> UV.Vector Float -> B.ByteString -> Int -> RefDefT -> Float -> Int -> Int -> Int -> UV.Vector Float
addLightMaps surf blockLights lightmap lightmapIndex newRefDef glModulate size idx maxIdx
    | idx >= maxIdx = blockLights
    | (surf^.msStyles) `B.index` idx == 255 = blockLights
    | otherwise =
        let rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral ((surf^.msStyles) `B.index` idx)))^.lsRGB
            scale0 = glModulate * (rgb^._x)
            scale1 = glModulate * (rgb^._y)
            scale2 = glModulate * (rgb^._z)
            (lightmapIndex', updates) = if all (== 1) [scale0, scale1, scale2]
                                            then setLightmap lightmap lightmapIndex 0 size []
                                            else setLightmapScale lightmap lightmapIndex scale0 scale1 scale2 0 size []
        in addLightMaps surf (blockLights UV.// updates) lightmap lightmapIndex' newRefDef glModulate size (idx + 1) maxIdx

updateLightMaps :: MSurfaceT -> UV.Vector Float -> B.ByteString -> Int -> RefDefT -> Float -> Int -> Int -> Int -> UV.Vector Float
updateLightMaps surf blockLights lightmap lightmapIndex newRefDef glModulate size idx maxIdx
    | idx >= maxIdx = blockLights
    | (surf^.msStyles) `B.index` idx == 255 = blockLights
    | otherwise =
        let rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral ((surf^.msStyles) `B.index` idx)))^.lsRGB
            scale0 = glModulate * (rgb^._x)
            scale1 = glModulate * (rgb^._y)
            scale2 = glModulate * (rgb^._z)
            (lightmapIndex', updates) = if all (== 1) [scale0, scale1, scale2]
                                            then updateLightmap blockLights lightmap lightmapIndex 0 size []
                                            else updateLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 0 size []
        in updateLightMaps surf (blockLights UV.// updates) lightmap lightmapIndex' newRefDef glModulate size (idx + 1) maxIdx

setLightmap :: B.ByteString -> Int -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
setLightmap lightmap lightmapIndex idx maxIdx acc
    | idx >= maxIdx = (lightmapIndex, acc)
    | otherwise =
        let a = fromIntegral (lightmap `B.index` (lightmapIndex + 0))
            b = fromIntegral (lightmap `B.index` (lightmapIndex + 1))
            c = fromIntegral (lightmap `B.index` (lightmapIndex + 2))
        in setLightmap lightmap (lightmapIndex + 3) (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

setLightmapScale :: B.ByteString -> Int -> Float -> Float -> Float -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
setLightmapScale lightmap lightmapIndex scale0 scale1 scale2 idx maxIdx acc
    | idx >= maxIdx = (lightmapIndex, acc)
    | otherwise =
        let a = scale0 * (fromIntegral (lightmap `B.index` (lightmapIndex + 0)))
            b = scale1 * (fromIntegral (lightmap `B.index` (lightmapIndex + 1)))
            c = scale2 * (fromIntegral (lightmap `B.index` (lightmapIndex + 2)))
        in setLightmapScale lightmap (lightmapIndex + 3) scale0 scale1 scale2 (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

updateLightmap :: UV.Vector Float -> B.ByteString -> Int -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
updateLightmap blockLights lightmap lightmapIndex idx maxIdx acc
    | idx >= maxIdx = (lightmapIndex, acc)
    | otherwise =
        let a = (blockLights UV.! (idx * 3 + 0)) + (fromIntegral (lightmap `B.index` (lightmapIndex + 0)))
            b = (blockLights UV.! (idx * 3 + 1)) + (fromIntegral (lightmap `B.index` (lightmapIndex + 1)))
            c = (blockLights UV.! (idx * 3 + 2)) + (fromIntegral (lightmap `B.index` (lightmapIndex + 2)))
        in updateLightmap blockLights lightmap (lightmapIndex + 3) (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

updateLightmapScale :: UV.Vector Float -> B.ByteString -> Int -> Float -> Float -> Float -> Int -> Int -> [(Int, Float)] -> (Int, [(Int, Float)])
updateLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 idx maxIdx acc
    | idx >= maxIdx = (lightmapIndex, acc)
    | otherwise =
        let a = (blockLights UV.! (idx * 3 + 0)) + scale0 * (fromIntegral (lightmap `B.index` (lightmapIndex + 0)))
            b = (blockLights UV.! (idx * 3 + 1)) + scale1 * (fromIntegral (lightmap `B.index` (lightmapIndex + 1)))
            c = (blockLights UV.! (idx * 3 + 2)) + scale2 * (fromIntegral (lightmap `B.index` (lightmapIndex + 2)))
        in updateLightmapScale blockLights lightmap (lightmapIndex + 3) scale0 scale1 scale2 (idx + 1) maxIdx ((idx * 3 + 0, a) : (idx * 3 + 1, b) : (idx * 3 + 2, c) : acc)

rAddDynamicLights :: MSurfaceT -> Quake ()
rAddDynamicLights = error "Light.rAddDynamicLights" -- TODO

putIntoTextureFormat :: SV.Vector Word8 -> Int -> Int -> Int -> Int -> Quake ()
putIntoTextureFormat buffer offset stride tmax smax = do
    monoLightMap <- fmap (^.cvString) glMonoLightMapCVar
    blockLights <- use (fastRenderAPIGlobals.frBlockLights)
    return $ runST $ do
        buffer' <- SV.unsafeThaw buffer
        pickBuildLightMap buffer' blockLights monoLightMap
        void (SV.unsafeFreeze buffer')
  where
    stride' = stride - (smax `shiftL` 2)
    pickBuildLightMap buffer' blockLights monoLightMap
        | monoLightMap `BC.index` 0 == '0' =
            doBuildLightMap buffer' blockLights 0 stride' offset 0 tmax 0 smax
        | otherwise =
            doBuildLightMapAlpha buffer' monoLightMap blockLights 0 stride' offset 0 tmax 0 smax

doBuildLightMap :: MSV.MVector s Word8 -> UV.Vector Float -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
doBuildLightMap buffer blockLights blp stride currentOffset i tmax j smax
    | i >= tmax = return ()
    | j >= smax = doBuildLightMap buffer blockLights blp stride (currentOffset + stride) (i + 1) tmax 0 smax
    | otherwise = do
        let r = (truncate (blockLights UV.! (blp + 0))) :: Int
            g = (truncate (blockLights UV.! (blp + 1))) :: Int
            b = (truncate (blockLights UV.! (blp + 2))) :: Int
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
            b'' = if brightest > 255 then truncate (fromIntegral b' * t) else fromIntegral b'
            a' = if brightest > 255 then truncate (fromIntegral a * t) else fromIntegral a
        MSV.write buffer (currentOffset + 0) r''
        MSV.write buffer (currentOffset + 1) g''
        MSV.write buffer (currentOffset + 2) b''
        MSV.write buffer (currentOffset + 3) a'
        doBuildLightMap buffer blockLights (blp + 3) stride (currentOffset + 4) i tmax (j + 1) smax

doBuildLightMapAlpha :: MSV.MVector s Word8 -> B.ByteString -> UV.Vector Float -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
doBuildLightMapAlpha buffer monoLightMap blockLights blp stride currentOffset i tmax j smax
    | i >= tmax = return ()
    | j >= smax = doBuildLightMapAlpha buffer monoLightMap blockLights blp stride (currentOffset + stride) (i + 1) tmax 0 smax
    | otherwise = do
        let r = (truncate (blockLights UV.! (blp + 0))) :: Int
            g = (truncate (blockLights UV.! (blp + 1))) :: Int
            b = (truncate (blockLights UV.! (blp + 2))) :: Int
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
            b'' = if brightest > 255 then truncate (fromIntegral b' * t) else b'
            a' = if brightest > 255 then truncate (fromIntegral a * t) else a
            -- So if we are doing alpha lightmaps we need to set the
            -- R, G, and B components to 0 and we need to set alpha to 1-alpha
            (r''', g''', b''', a'') = updateRGBA monoLightMap r'' g'' b'' a'
        MSV.write buffer (currentOffset + 0) r'''
        MSV.write buffer (currentOffset + 1) g'''
        MSV.write buffer (currentOffset + 2) b'''
        MSV.write buffer (currentOffset + 3) a''
        doBuildLightMapAlpha buffer monoLightMap blockLights (blp + 3) stride (currentOffset + stride) i tmax (j + 1) smax

updateRGBA :: B.ByteString -> Int -> Int -> Int -> Int -> (Word8, Word8, Word8, Word8)
updateRGBA monoLightMap r g b a
    | c == 'L' || c == 'I' = (fromIntegral a, 0, 0, fromIntegral a)
    | c == 'C'= 
        let a' = 255 - ((fromIntegral (r + g + b)) / 3)
            af = a' / 255 :: Float
            r' = fromIntegral r * af
            g' = fromIntegral g * af
            b' = fromIntegral b * af
        in (truncate r', truncate g', truncate b', truncate a')
    | otherwise = (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral (255 - a))
  where
    c = monoLightMap `BC.index` 0
