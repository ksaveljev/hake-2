{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Light
    ( rBuildLightMap
    , rLightPoint
    , rMarkLights
    , rPushDLights
    , rRenderDLights
    , rSetCacheState
    ) where

import           Control.Lens                 (use, ix, (^.), (.=), (&), (.~), (%~), _1, _2)
import           Control.Monad                (void, when, unless)
import           Control.Monad.ST             (ST, runST)
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Maybe                   (fromMaybe, isNothing)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV
import           Data.Word                    (Word8)
import qualified Graphics.GL                  as GL
import           Linear                       (V3(..), dot, norm, _x, _y, _z, _xyz, _w)

import           Client.DLightT
import           Client.EntityT
import           Client.LightStyleT
import           Client.RefDefT
import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import qualified QCommon.Com                  as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Render.MNodeT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Types

dLightCutoff :: Float
dLightCutoff = 64

rPushDLights :: Quake ()
rPushDLights = do
    flashBlend <- fmap (^.cvValue) glFlashBlendCVar
    when (flashBlend == 0) $ do
        frameCount <- use (fastRenderAPIGlobals.frFrameCount)
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
        fastRenderAPIGlobals.frDLightFrameCount .= frameCount + 1 -- because the count hasn't advanced yet for this frame
        doPushDLights newRefDef worldModelRef
  where
    doPushDLights _ Nothing =
        Com.fatalError "Light.rPushDLights worldModelRef is Nothing"
    doPushDLights newRefDef (Just worldModelRef) = do
        worldModel <- readRef worldModelRef
        mapM_ (markLight newRefDef worldModelRef worldModel) [0..(newRefDef^.rdNumDLights)-1]
    markLight newRefDef worldModelRef worldModel idx = do
        rMarkLights worldModelRef worldModel ((newRefDef^.rdDLights) V.! idx) (1 `shiftL` idx) (MNodeChildRef (Ref Constants.noParent 0))

rMarkLights :: Ref' ModelT -> ModelT -> DLightT -> Int -> MNodeChild -> Quake ()
rMarkLights _ _ _ _ (MLeafChildRef _) = return ()
rMarkLights modelRef model light bit (MNodeChildRef (Ref _ idx)) = do
    splitPlane <- readRef (node^.mnPlane)
    doMarkLights ((light^.dlOrigin) `dot` (splitPlane^.cpNormal) - (splitPlane^.cpDist))
  where
    node = (model^.mNodes) V.! idx
    doMarkLights dist
        | dist > (light^.dlIntensity) - dLightCutoff = rMarkLights modelRef model light bit (node^.mnChildren._1)
        | dist < dLightCutoff - (light^.dlIntensity) = rMarkLights modelRef model light bit (node^.mnChildren._2)
        | otherwise = do
            mapM_ (markPolygon modelRef model node light bit) [0..(node^.mnNumSurfaces)-1]
            rMarkLights modelRef model light bit (node^.mnChildren._1)
            rMarkLights modelRef model light bit (node^.mnChildren._2)

markPolygon :: Ref' ModelT -> ModelT -> MNodeT -> DLightT -> Int -> Int -> Quake ()
markPolygon modelRef model node light bit idx =
    proceedMarkPolygon (surf^.msPlane)
  where
    surfIdx = ((node^.mnFirstSurface) + idx)
    surf = (model^.mSurfaces) V.! surfIdx
    proceedMarkPolygon Nothing = Com.fatalError "Light.markPolygon surf^.msPlane is Nothing"
    proceedMarkPolygon (Just planeRef) = do
        plane <- readRef planeRef
        doMarkPolygon modelRef surf surfIdx plane light bit

doMarkPolygon :: Ref' ModelT -> MSurfaceT -> Int -> CPlaneT -> DLightT -> Int -> Quake ()
doMarkPolygon modelRef surf surfIdx plane light bit
    | (surf^.msFlags) .&. Constants.surfPlaneBack /= sidebit =
        return ()
    | otherwise = do
        dLightFrameCount <- use (fastRenderAPIGlobals.frDLightFrameCount)
        when ((surf^.msDLightFrame) /= dLightFrameCount) $
            modifyRef modelRef (\v -> v & mSurfaces.ix surfIdx %~ (\s -> s & msDLightBits .~ 0
                                                                           & msDLightFrame .~ dLightFrameCount))
        modifyRef modelRef (\v -> v & mSurfaces.ix surfIdx %~ (\s -> s & msDLightBits %~ (.|. bit)))
  where
    dist = (light^.dlOrigin) `dot` (plane^.cpNormal) - (plane^.cpDist)
    sidebit | dist >= 0 = 0
            | otherwise = Constants.surfPlaneBack

rRenderDLights :: Quake ()
rRenderDLights = do
    flashBlend <- fmap (^.cvValue) glFlashBlendCVar
    unless (flashBlend == 0) $ do
        frameCount <- use (fastRenderAPIGlobals.frFrameCount)
        fastRenderAPIGlobals.frDLightFrameCount .= frameCount + 1 -- because the count hasn't advanced yet for this frame
        request $ do
            GL.glDepthMask (fromIntegral GL.GL_FALSE)
            GL.glDisable GL.GL_TEXTURE_2D
            GL.glShadeModel GL.GL_SMOOTH
            GL.glEnable GL.GL_BLEND
            GL.glBlendFunc GL.GL_ONE GL.GL_ONE
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        mapM_ (\idx -> rRenderDLight ((newRefDef^.rdDLights) V.! idx)) [0..(newRefDef^.rdNumDLights)-1]
        request $ do
            GL.glColor3f 1 1 1
            GL.glDisable GL.GL_BLEND
            GL.glEnable GL.GL_TEXTURE_2D
            GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
            GL.glDepthMask (fromIntegral GL.GL_FALSE)

rRenderDLight :: DLightT -> Quake ()
rRenderDLight light = do
    -- origin <- use (fastRenderAPIGlobals.frOrigin)
    vpn <- use (fastRenderAPIGlobals.frVPn)
    vup <- use (fastRenderAPIGlobals.frVUp)
    vright <- use (fastRenderAPIGlobals.frVRight)
    let rad = (light^.dlIntensity) * 0.35
        -- v = (light^.dlOrigin) - origin -- not used in Quake2 original, section commented out
    request $ do
        GL.glBegin GL.GL_TRIANGLE_FAN
        GL.glColor3f (realToFrac ((light^.dlColor._x) * 0.2)) (realToFrac ((light^.dlColor._y) * 0.2)) (realToFrac ((light^.dlColor._z) * 0.2))
    let v' = (light^.dlOrigin) - fmap (* rad) vpn
    request $ do
        GL.glVertex3f (realToFrac (v'^._x)) (realToFrac (v'^._y)) (realToFrac (v'^._z))
        GL.glColor3f 0 0 0
    addVertex vright vup rad 16 0
    request GL.glEnd

  where
    addVertex :: V3 Float -> V3 Float -> Float -> Int -> Int -> Quake ()
    addVertex vright vup rad idx minIdx
        | idx < minIdx = return ()
        | otherwise = do
            let a = fromIntegral idx / 16 * pi * 2
                v = (light^.dlOrigin) + fmap (* ((cos a) * rad)) vright + fmap (* ((sin a) * rad)) vup
            request $
                GL.glVertex3f (realToFrac (v^._x)) (realToFrac (v^._y)) (realToFrac (v^._z))
            addVertex vright vup rad (idx - 1) minIdx

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint p = do
    worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
    maybe worldModelError (lightPoint p) worldModelRef
  where
    worldModelError = do
        Com.fatalError "Light.rLightPoint worldModelRef is Nothing"
        return (V3 0 0 0)

lightPoint :: V3 Float -> Ref' ModelT -> Quake (V3 Float)
lightPoint p@(V3 a b c) worldModelRef = do
    worldModel <- readRef worldModelRef
    maybe (return (V3 1 1 1)) doLightPoint (worldModel^.mLightdata)
  where
    doLightPoint _ = do
        r <- recursiveLightPoint worldModelRef (MNodeChildRef (Ref Constants.noParent 0)) p (V3 a b (c - 2048))
        color <- getColor r
        -- add dynamic lights
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        currentEntityRef <- maybe entityError return =<< use (fastRenderAPIGlobals.frCurrentEntity)
        currentEntity <- readCurrentEntity currentEntityRef
        let color' = addDynamicLights currentEntity newRefDef color 0 (newRefDef^.rdNumDLights)
        modulate <- fmap (^.cvValue) glModulateCVar
        return (fmap (* modulate) color')
    getColor r
        | r == -1   = use (globals.gVec3Origin)
        | otherwise = use (fastRenderAPIGlobals.frPointColor)
    entityError = do
        Com.fatalError "Light.rLightPoint fastRenderAPIGlobals.frCurrentEntity is Nothing"
        return (NewEntity newEntityT)

addDynamicLights :: EntityT -> RefDefT -> V3 Float -> Int -> Int -> V3 Float
addDynamicLights currentEntity newRefDef color idx maxIdx
    | idx >= maxIdx = color
    | otherwise =
        let dlight = (newRefDef^.rdDLights) V.! idx
            end = (currentEntity^.eOrigin) - (dlight^.dlOrigin)
            add = ((dlight^.dlIntensity) - norm end) * (1 / 256)
            color' | add > 0   = color + fmap (* add) (dlight^.dlColor)
                   | otherwise = color
        in addDynamicLights currentEntity newRefDef color' (idx + 1) maxIdx

recursiveLightPoint :: Ref' ModelT -> MNodeChild -> V3 Float -> V3 Float -> Quake Int
recursiveLightPoint _ (MLeafChildRef _) _ _ = return (-1) -- didn't hit anything
recursiveLightPoint worldModelRef (MNodeChildRef (Ref _ nodeIdx)) start end = do
    -- calculate mid point
    worldModel <- readRef worldModelRef
    let node = (worldModel^.mNodes) V.! nodeIdx
        planeRef = node^.mnPlane
    plane <- readRef planeRef
    let front = start `dot` (plane^.cpNormal) - (plane^.cpDist)
        back = end `dot` (plane^.cpNormal) - (plane^.cpDist)
        side = front < 0
        sideIndex = if side then _2 else _1
        sideIndex2 = if side then _1 else _2
    doRecursiveLightPoint node front back sideIndex sideIndex2
  where
    doRecursiveLightPoint node front back sideIndex sideIndex2
        | (back < 0) == (front < 0) =
            recursiveLightPoint worldModelRef (node^.mnChildren.sideIndex) start end
        | otherwise = do
            let frac = front / (front - back)
                mid = start + fmap (* frac) (end - start)
            -- go down front side
            r <- recursiveLightPoint worldModelRef (node^.mnChildren.sideIndex) start mid
            proceedRecursiveLightPoint node front back mid r sideIndex2
    proceedRecursiveLightPoint node front back mid r sideIndex2
        | r >= 0 = return r
        | (back < 0) == (front < 0) = return (-1)
        | otherwise = do
            -- check for impact on this node
            fastRenderAPIGlobals.frLightSpot .= mid
            worldModel <- readRef worldModelRef
            r' <- calcPointColor worldModel mid (node^.mnFirstSurface) 0 (node^.mnNumSurfaces)
            maybe (recursiveLightPoint worldModelRef (node^.mnChildren.sideIndex2) mid end) return r'

calcPointColor :: ModelT -> V3 Float -> Int -> Int -> Int -> Quake (Maybe Int)
calcPointColor worldModel mid surfIndex idx maxIdx
    | idx >= maxIdx = return Nothing
    | ((worldModel^.mSurfaces) V.! surfIndex)^.msFlags .&. (Constants.surfDrawTurb .|. Constants.surfDrawSky) /= 0 =
        -- no lightmaps
        calcPointColor worldModel mid (surfIndex + 1) (idx + 1) maxIdx
    | otherwise = do
        let surf = (worldModel^.mSurfaces) V.! surfIndex
            Ref _ texIdx = surf^.msTexInfo
            tex = (worldModel^.mTexInfo) V.! texIdx
            s = truncate (mid `dot` (tex^.mtiVecs._1._xyz) + (tex^.mtiVecs._1._w)) :: Int
            t = truncate (mid `dot` (tex^.mtiVecs._2._xyz) + (tex^.mtiVecs._2._w)) :: Int
        doCalcPointColor surf s t
  where
    doCalcPointColor surf s t
        | s < fromIntegral (surf^.msTextureMins._1) || t < fromIntegral (surf^.msTextureMins._2) =
            calcPointColor worldModel mid (surfIndex + 1) (idx + 1) maxIdx
        | otherwise = do
            let ds = s - fromIntegral (surf^.msTextureMins._1)
                dt = t - fromIntegral (surf^.msTextureMins._2)
            setPointColor surf ds dt
    setPointColor surf ds dt
        | ds > fromIntegral (surf^.msExtents._1) || dt > fromIntegral (surf^.msExtents._2) =
            calcPointColor worldModel mid (surfIndex + 1) (idx + 1) maxIdx
        | isNothing (surf^.msSamples) =
            return (Just 0)
        | otherwise = do
            let ds' = ds `shiftR` 4
                dt' = dt `shiftR` 4
            lightmap <- getLightmap (surf^.msSamples) (worldModel^.mLightdata)
            v3o <- use (globals.gVec3Origin)
            newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
            modulate <- fmap (^.cvValue) glModulateCVar
            let lightmapIndex = 3 * (dt' * ((fromIntegral (surf^.msExtents._1) `shiftR` 4) + 1) + ds')
                pointColor = calculate surf lightmap newRefDef modulate lightmapIndex v3o 0 Constants.maxLightMaps
            fastRenderAPIGlobals.frPointColor .= pointColor
            return (Just 1)
    getLightmap Nothing _ = do
        Com.fatalError "Light.addUpdateLightMaps surf^.msSamples cannot be Nothing"
        return B.empty
    getLightmap _ Nothing = do
        Com.fatalError "Light.addUpdateLightMaps lightData cannot be Nothing"
        return B.empty
    getLightmap (Just offset) (Just lightmap) = return (B.drop offset lightmap)

calculate :: MSurfaceT -> B.ByteString -> RefDefT -> Float -> Int -> V3 Float -> Int -> Int -> V3 Float
calculate surf lightmap newRefDef modulateValue lightmapIndex pointColor idx maxIdx
    | idx >= maxIdx = pointColor
    | (surf^.msStyles) `B.index` idx == 0xFF = pointColor
    | otherwise =
        let rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral ((surf^.msStyles) `B.index` idx)))^.lsRGB
            scale = fmap (* modulateValue) rgb
            a = (pointColor^._x) + (fromIntegral (lightmap `B.index` (lightmapIndex + 0))) * (scale^._x) * (1 / 255)
            b = (pointColor^._y) + (fromIntegral (lightmap `B.index` (lightmapIndex + 1))) * (scale^._y) * (1 / 255)
            c = (pointColor^._z) + (fromIntegral (lightmap `B.index` (lightmapIndex + 2))) * (scale^._z) * (1 / 255)
            lightmapIndex' = lightmapIndex + 3 * ((fromIntegral (surf^.msExtents._1) `shiftR` 4) + 1) * ((fromIntegral (surf^.msExtents._2) `shiftR` 4) + 1)
        in calculate surf lightmap newRefDef modulateValue lightmapIndex' (V3 a b c) (idx + 1) maxIdx

rSetCacheState :: Ref' MSurfaceT -> Quake ()
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
        MUV.write lights idx (lightStyle^.lsWhite)
        cacheLights newRefDef surf lights (idx + 1) maxIdx
  where
    style = (surf^.msStyles) `B.index` idx
    lightStyle = (newRefDef^.rdLightStyles) V.! (fromIntegral style)

rBuildLightMap :: Ref' MSurfaceT -> Maybe B.ByteString -> SV.Vector Word8 -> Int -> Int -> Quake ()
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
        -- fastRenderAPIGlobals.frBlockLights %= (UV.// (zip [0..size * 3 - 1] (repeat 255)))
        fastRenderAPIGlobals.frBlockLights .= UV.replicate (34 * 34 * 3) 255
        return True
    checkLightData _ = return False

addUpdateLightMaps :: MSurfaceT -> Maybe B.ByteString -> Int -> Quake ()
addUpdateLightMaps surf lightData size = do
    lightmap <- getLightmap (surf^.msSamples) lightData
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    glModulate <- fmap (^.cvValue) glModulateCVar
    blockLights <- use (fastRenderAPIGlobals.frBlockLights)
    let !blockLights' = doAddUpdateLightMaps blockLights lightmap newRefDef glModulate
    -- fastRenderAPIGlobals.frBlockLights .= doAddUpdateLightMaps blockLights lightmap newRefDef glModulate
    fastRenderAPIGlobals.frBlockLights .= blockLights'
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
        | numMaps == 1 = runST $ do
            bl <- UV.unsafeThaw blockLights
            addLightMaps surf bl lightmap 0 newRefDef glModulate size 0 Constants.maxLightMaps
            UV.unsafeFreeze bl
        | otherwise = runST $ do
            bl <- UV.unsafeThaw blockLights
            MUV.set bl 0
            updateLightMaps surf bl lightmap 0 newRefDef glModulate size 0 Constants.maxLightMaps
            UV.unsafeFreeze bl

addLightMaps :: MSurfaceT -> MUV.STVector s Float -> B.ByteString -> Int -> RefDefT -> Float -> Int -> Int -> Int -> ST s ()
addLightMaps surf blockLights lightmap lightmapIndex newRefDef glModulate size idx maxIdx
    | idx >= maxIdx = return ()
    | (surf^.msStyles) `B.index` idx == 255 = return ()
    | otherwise = do
        let !rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral ((surf^.msStyles) `B.index` idx)))^.lsRGB
            !scale0 = glModulate * (rgb^._x)
            !scale1 = glModulate * (rgb^._y)
            !scale2 = glModulate * (rgb^._z)
        lightmapIndex' <- doSetLightmap scale0 scale1 scale2
        addLightMaps surf blockLights lightmap lightmapIndex' newRefDef glModulate size (idx + 1) maxIdx
  where
    doSetLightmap scale0 scale1 scale2
        | all (== 1) [scale0, scale1, scale2] = setLightmap blockLights lightmap lightmapIndex 0 size
        | otherwise = setLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 0 size

updateLightMaps :: MSurfaceT -> MUV.STVector s Float -> B.ByteString -> Int -> RefDefT -> Float -> Int -> Int -> Int -> ST s ()
updateLightMaps surf blockLights lightmap lightmapIndex newRefDef glModulate size idx maxIdx
    | idx >= maxIdx = return ()
    | (surf^.msStyles) `B.index` idx == 255 = return ()
    | otherwise = do
        let !rgb = ((newRefDef^.rdLightStyles) V.! (fromIntegral ((surf^.msStyles) `B.index` idx)))^.lsRGB
            !scale0 = glModulate * (rgb^._x)
            !scale1 = glModulate * (rgb^._y)
            !scale2 = glModulate * (rgb^._z)
        lightmapIndex' <- doUpdateLightmap scale0 scale1 scale2
        updateLightMaps surf blockLights lightmap lightmapIndex' newRefDef glModulate size (idx + 1) maxIdx
  where
    doUpdateLightmap scale0 scale1 scale2
        | all (== 1) [scale0, scale1, scale2] = updateLightmap blockLights lightmap lightmapIndex 0 size
        | otherwise = updateLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 0 size

setLightmap :: MUV.STVector s Float -> B.ByteString -> Int -> Int -> Int -> ST s Int
setLightmap blockLights lightmap lightmapIndex idx maxIdx
    | idx >= maxIdx = return lightmapIndex
    | otherwise = do
        let !a = fromIntegral (lightmap `B.index` (lightmapIndex + 0))
            !b = fromIntegral (lightmap `B.index` (lightmapIndex + 1))
            !c = fromIntegral (lightmap `B.index` (lightmapIndex + 2))
        MUV.write blockLights (idx * 3 + 0) a
        MUV.write blockLights (idx * 3 + 1) b
        MUV.write blockLights (idx * 3 + 2) c
        setLightmap blockLights lightmap (lightmapIndex + 3) (idx + 1) maxIdx

setLightmapScale :: MUV.STVector s Float -> B.ByteString -> Int -> Float -> Float -> Float -> Int -> Int -> ST s Int
setLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 idx maxIdx
    | idx >= maxIdx = return lightmapIndex
    | otherwise = do
        let !a = scale0 * (fromIntegral (lightmap `B.index` (lightmapIndex + 0)))
            !b = scale1 * (fromIntegral (lightmap `B.index` (lightmapIndex + 1)))
            !c = scale2 * (fromIntegral (lightmap `B.index` (lightmapIndex + 2)))
        MUV.write blockLights (idx * 3 + 0) a
        MUV.write blockLights (idx * 3 + 1) b
        MUV.write blockLights (idx * 3 + 2) c
        setLightmapScale blockLights lightmap (lightmapIndex + 3) scale0 scale1 scale2 (idx + 1) maxIdx

updateLightmap :: MUV.STVector s Float -> B.ByteString -> Int -> Int -> Int -> ST s Int
updateLightmap blockLights lightmap lightmapIndex idx maxIdx
    | idx >= maxIdx = return lightmapIndex
    | otherwise = do
        let !a = fromIntegral (lightmap `B.index` (lightmapIndex + 0))
            !b = fromIntegral (lightmap `B.index` (lightmapIndex + 1))
            !c = fromIntegral (lightmap `B.index` (lightmapIndex + 2))
        MUV.modify blockLights (a +) (idx * 3 + 0)
        MUV.modify blockLights (b +) (idx * 3 + 1)
        MUV.modify blockLights (c +) (idx * 3 + 2)
        updateLightmap blockLights lightmap (lightmapIndex + 3) (idx + 1) maxIdx

updateLightmapScale :: MUV.STVector s Float -> B.ByteString -> Int -> Float -> Float -> Float -> Int -> Int -> ST s Int
updateLightmapScale blockLights lightmap lightmapIndex scale0 scale1 scale2 idx maxIdx
    | idx >= maxIdx = return lightmapIndex
    | otherwise = do
        let !a = scale0 * (fromIntegral (lightmap `B.index` (lightmapIndex + 0)))
            !b = scale1 * (fromIntegral (lightmap `B.index` (lightmapIndex + 1)))
            !c = scale2 * (fromIntegral (lightmap `B.index` (lightmapIndex + 2)))
        MUV.modify blockLights (a +) (idx * 3 + 0)
        MUV.modify blockLights (b +) (idx * 3 + 1)
        MUV.modify blockLights (c +) (idx * 3 + 2)
        updateLightmapScale blockLights lightmap (lightmapIndex + 3) scale0 scale1 scale2 (idx + 1) maxIdx

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
