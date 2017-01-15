{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Surf
    ( glBeginBuildingLightmaps
    , glBuildPolygonFromSurface
    , glCreateSurfaceLightmap
    , glEndBuildingLightmaps
    , rDrawAlphaSurfaces
    , rDrawBrushModel
    , rDrawWorld
    , rMarkLeaves
    ) where

import           Control.Lens                 (use, ix, (^.), (.=), (%=), (+=), (&), (.~), (+~))
import           Control.Lens                 (_1, _2)
import           Control.Monad                (when, unless)
import           Control.Monad.ST             (runST)
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Char                    (toUpper)
import           Data.Maybe                   (isNothing)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV
import           Data.Word                    (Word8)
import qualified Graphics.GL                  as GL
import           Linear                       (V3(..), dot, _x, _y, _z, _xyz, _w)

import           Client.EntityT
import           Client.RefDefT
import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import qualified QCommon.Com                  as Com
import           QCommon.CVarVariables
import           Render.Fast.GLLightMapStateT
import qualified Render.Fast.Image            as Image
import qualified Render.Fast.Light            as Light
import qualified Render.Fast.Shared           as Model
import qualified Render.Fast.Polygon          as Polygon
import qualified Render.Fast.Warp             as Warp
import           Render.GLPolyT
import           Render.GLStateT
import           Render.ImageT
import           Render.MEdgeT
import           Render.MLeafT
import           Render.MNodeT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Render.MVertexT
import           QuakeIOState
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary                  (encode)
import qualified Util.Math3D                  as Math3D

-- TODO: Vector or ByteString?
dummy :: SV.Vector Word8
dummy = SV.replicate (4 * 128 * 128) 0

lightmapBytes :: Int
lightmapBytes = 4

glLightmapFormat :: GL.GLenum
glLightmapFormat = GL.GL_RGBA

rDrawAlphaSurfaces :: Quake ()
rDrawAlphaSurfaces = error "Surf.rDrawAlphaSurfaces" -- TODO

rMarkLeaves :: Quake ()
rMarkLeaves = do
    oldViewCluster <- use (fastRenderAPIGlobals.frOldViewCluster)
    oldViewCluster2 <- use (fastRenderAPIGlobals.frOldViewCluster2)
    viewCluster <- use (fastRenderAPIGlobals.frViewCluster)
    viewCluster2 <- use (fastRenderAPIGlobals.frViewCluster2)
    noVis <- fmap (^.cvValue) noVisCVar
    unless (oldViewCluster == viewCluster && oldViewCluster2 == viewCluster2 && noVis == 0 && viewCluster /= -1) $ do
        -- TODO: implement this development stuff
        -- if (gl_lockpvs.value != 0)
        --     return;
        fastRenderAPIGlobals %= (\v -> v & frVisFrameCount +~ 1
                                         & frOldViewCluster .~ viewCluster
                                         & frOldViewCluster2 .~ viewCluster2)
        worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
        maybe worldModelError (getAndProceed noVis viewCluster viewCluster2) worldModelRef
  where
    worldModelError = Com.fatalError "Surf.rMarkLeaves worldModelRef is Nothing"
    getAndProceed noVis viewCluster viewCluster2 worldModelRef = do
        worldModel <- readRef worldModelRef
        proceedMarkLeaves worldModelRef worldModel noVis viewCluster viewCluster2

proceedMarkLeaves :: Ref' ModelT -> ModelT -> Float -> Int -> Int -> Quake ()
proceedMarkLeaves worldModelRef worldModel noVis viewCluster viewCluster2
    | noVis /= 0 || viewCluster == -1 || isNothing (worldModel^.mVis) = do
        visFrameCount <- use (fastRenderAPIGlobals.frVisFrameCount)
        mapM_ (setLeafVisFrame visFrameCount) [0..(worldModel^.mNumLeafs)-1]
        mapM_ (setNodeVisFrame visFrameCount) [0..(worldModel^.mNumNodes)-1]
    | otherwise = do
        vis <- Model.clusterPVS viewCluster worldModel
        vis' <- checkCombineClusters vis
        visFrameCount <- use (fastRenderAPIGlobals.frVisFrameCount)
        mapM_ (markLeaf worldModelRef worldModel vis' visFrameCount) [0..(worldModel^.mNumLeafs)-1]
  where
    checkCombineClusters vis
        | viewCluster2 /= viewCluster = combineClusters worldModel vis viewCluster2
        | otherwise = return vis
    setLeafVisFrame visFrameCount idx =
        modifyRef worldModelRef (\v -> v & mLeafs.ix idx.mlVisFrame .~ visFrameCount)
    setNodeVisFrame visFrameCount idx =
        modifyRef worldModelRef (\v -> v & mNodes.ix idx.mnVisFrame .~ visFrameCount)

combineClusters :: ModelT -> B.ByteString -> Int -> Quake B.ByteString
combineClusters worldModel vis viewCluster2 = do
    vis' <- Model.clusterPVS viewCluster2 worldModel
    return (B.pack (B.zipWith (\a b -> a .|. b) (B.take c fatvis) (B.take c vis')) `B.append` (B.drop c fatvis))
  where
    len = ((worldModel^.mNumLeafs) + 7) `shiftR` 3
    fatvis = B.take len vis `B.append` B.replicate ((Constants.maxMapLeafs `div` 8) - len) 0
    c = (((worldModel^.mNumLeafs) + 31) `shiftR` 5) `shiftL` 2

-- TODO: verify that worldModel doesn't need to be refreshed upon each call
markLeaf :: Ref' ModelT -> ModelT -> B.ByteString -> Int -> Int -> Quake ()
markLeaf worldModelRef worldModel vis visFrameCount idx
    | cluster == -1 = return ()
    | (vis `B.index` (cluster `shiftR` 3)) .&. (1 `shiftL` (cluster .&. 7)) /= 0 = do
        when ((leaf^.mlVisFrame) /= visFrameCount) $ do
            modifyRef worldModelRef (\v -> v & mLeafs.ix idx.mlVisFrame .~ visFrameCount)
            markNode worldModelRef worldModel (leaf^.mlParent) visFrameCount
    | otherwise = return ()
  where
    leaf = (worldModel^.mLeafs) V.! idx
    cluster = leaf^.mlCluster

-- TODO: verify that worldModel doesn't need to be refreshed upon each call
markNode :: Ref' ModelT -> ModelT -> Maybe (Ref' MNodeT) -> Int -> Quake ()
markNode _ _ Nothing _ = return ()
markNode worldModelRef worldModel (Just (Ref idx)) visFrameCount
    | node^.mnVisFrame == visFrameCount =
        return ()
    | otherwise = do
        modifyRef worldModelRef (\v -> v & mNodes.ix idx.mnVisFrame .~ visFrameCount)
        markNode worldModelRef worldModel (node^.mnParent) visFrameCount
  where
    node = (worldModel^.mNodes) V.! idx

glBeginBuildingLightmaps :: Ref' ModelT -> Quake ()
glBeginBuildingLightmaps _ = do
    fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate Constants.blockWidth 0
    fastRenderAPIGlobals.frFrameCount .= 1 -- no dlightcache
    Image.glEnableMultiTexture True
    Image.glSelectTexture =<< use (fastRenderAPIGlobals.frTexture1)
    fastRenderAPIGlobals.frNewRefDef.rdLightStyles .= lightStyles
    fastRenderAPIGlobals.frGLState.glsLightmapTextures %= (\v -> if v == 0 then Constants.texNumLightmaps else v)
    fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture .= 1
    pickInternalFormat =<< glMonoLightMapCVar
    glState <- use (fastRenderAPIGlobals.frGLState)
    lms <- use (fastRenderAPIGlobals.frGLLms)
    Image.glBind (glState^.glsLightmapTextures)
    request (initializeDynamicLightmap lms)
  where
    lightStyles = V.replicate Constants.maxLightStyles LightStyleT { _lsRGB = V3 1 1 1, _lsWhite = 3 }
    initializeDynamicLightmap lms = do
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
        io $ SV.unsafeWith dummy $ \ptr ->
            GL.glTexImage2D GL.GL_TEXTURE_2D
                            0
                            (fromIntegral (lms^.lmsInternalFormat))
                            (fromIntegral Constants.blockWidth)
                            (fromIntegral Constants.blockHeight)
                            0
                            (fromIntegral glLightmapFormat)
                            GL.GL_UNSIGNED_BYTE
                            ptr

pickInternalFormat :: CVarT -> Quake ()
pickInternalFormat glMonoLightMap = do
    v <- checkFormat format
    fastRenderAPIGlobals.frGLLms.lmsInternalFormat .= v
  where
    format = toUpper (BC.index (glMonoLightMap^.cvString) 0)
    checkFormat 'A' = use (fastRenderAPIGlobals.frGLTexAlphaFormat)
    checkFormat 'C' = use (fastRenderAPIGlobals.frGLTexAlphaFormat)
    checkFormat 'I' = return Constants.glIntensity8
    checkFormat 'L' = return Constants.glLuminance8
    checkFormat _ = use (fastRenderAPIGlobals.frGLTexSolidFormat)

glEndBuildingLightmaps :: Quake ()
glEndBuildingLightmaps = do
    lmUploadBlock False
    Image.glEnableMultiTexture False

lmUploadBlock :: Bool -> Quake ()
lmUploadBlock dynamic = do
    lms <- use (fastRenderAPIGlobals.frGLLms)
    glState <- use (fastRenderAPIGlobals.frGLState)
    Image.glBind ((glState^.glsLightmapTextures) + (pickTexture lms))
    request $ do
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
        io (doUploadBlock dynamic lms)
    fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture += 1
    checkLightmaps =<< use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture)
  where
    pickTexture lms
        | dynamic = 0
        | otherwise = lms^.lmsCurrentLightmapTexture
    checkLightmaps v
        | v == 128 = -- TODO: 128 should be a constant here but there is some name clashing
            Com.comError Constants.errDrop "LM_UploadBlock() - MAX_LIGHTMAPS exceeded\n"
        | otherwise = return ()

doUploadBlock :: Bool -> GLLightMapStateT -> IO ()
doUploadBlock dynamic lms
    | dynamic =
        SV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
            GL.glTexSubImage2D GL.GL_TEXTURE_2D
                               0
                               0
                               0
                               (fromIntegral Constants.blockWidth)
                               (fromIntegral height)
                               glLightmapFormat
                               GL.GL_UNSIGNED_BYTE
                               ptr
    | otherwise =
        SV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
            GL.glTexImage2D GL.GL_TEXTURE_2D
                            0
                            (fromIntegral (lms^.lmsInternalFormat))
                            (fromIntegral Constants.blockWidth)
                            (fromIntegral Constants.blockHeight)
                            0
                            glLightmapFormat
                            GL.GL_UNSIGNED_BYTE
                            ptr
  where
    h = UV.maximum (lms^.lmsAllocated)
    height = max 0 h

glCreateSurfaceLightmap :: Ref' MSurfaceT -> Maybe B.ByteString -> Quake ()
glCreateSurfaceLightmap surfaceRef lightData = do
    surface <- readRef surfaceRef
    when ((surface^.msFlags) .&. (Constants.surfDrawSky .|. Constants.surfDrawTurb) == 0) $
        doCreateSurfaceLightmap surfaceRef surface lightData

doCreateSurfaceLightmap :: Ref' MSurfaceT -> MSurfaceT -> Maybe B.ByteString -> Quake ()
doCreateSurfaceLightmap surfaceRef surface lightData = do
    pos <- tryAllocBlock
    lightmapTexture <- use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture)
    modifyRef surfaceRef (\v -> v & msLightS .~ (pos^._1)
                                  & msLightT .~ (pos^._2)
                                  & msLightmapTextureNum .~ lightmapTexture)
    Light.rSetCacheState surfaceRef
    buffer <- use (fastRenderAPIGlobals.frGLLms.lmsLightmapBuffer)
    Light.rBuildLightMap surfaceRef lightData buffer (((pos^._2) * Constants.blockWidth + (pos^._1)) * lightmapBytes) (Constants.blockWidth * lightmapBytes)
  where
    smax = fromIntegral (((surface^.msExtents._1) `shiftR` 4) + 1)
    tmax = fromIntegral (((surface^.msExtents._2) `shiftR` 4) + 1)
    tryAllocBlock = do
        (ok, pos) <- lmAllocBlock smax tmax (surface^.msLightS, surface^.msLightT)
        retryAllocBlock ok pos
    retryAllocBlock True pos = return pos
    retryAllocBlock False _ = do
        lmUploadBlock False
        lmInitBlock
        (ok, pos) <- lmAllocBlock smax tmax (surface^.msLightS, surface^.msLightT)
        unless ok $
            Com.fatalError (B.concat ["Consecutive calls to LM_AllocBlock(", encode smax, ",", encode tmax, ") failed\n"])
        return pos

glBuildPolygonFromSurface :: Ref' MSurfaceT -> Quake ()
glBuildPolygonFromSurface surfRef = do
    surf <- readRef surfRef
    model <- readCurrentModel
    image <- readTextureImage surf
    buildPolygonFromSurface surfRef surf model image
  where
    readCurrentModel = do
        modelRef <- use (fastRenderAPIGlobals.frCurrentModel)
        maybe currentModelError readRef modelRef
    currentModelError = do
        Com.fatalError "Surf.glBuildPolygonFromSurface current model is Nothing"
        return newModelT
    readTextureImage surf = do
        texInfo <- readRef (surf^.msTexInfo)
        maybe texInfoImageError readRef (texInfo^.mtiImage)
    texInfoImageError = do
        Com.fatalError "Surf.glBuildPolygonFromSurface tex info image is Nothing"
        return (newImageT (-1))

buildPolygonFromSurface :: Ref' MSurfaceT -> MSurfaceT -> ModelT -> ImageT -> Quake ()
buildPolygonFromSurface surfRef surf model image = do
    polyRef <- Polygon.create lNumVerts
    modifyRef polyRef (\v -> v & glpNext .~ (surf^.msPolys)
                               & glpFlags .~ (surf^.msFlags))
    modifyRef surfRef (\v -> v & msPolys .~ Just polyRef)
    doStuffWithVerts surf model image polyRef 0 lNumVerts
  where
    lNumVerts = surf^.msNumEdges

doStuffWithVerts :: MSurfaceT -> ModelT -> ImageT -> Ref' GLPolyT -> Int -> Int -> Quake ()
doStuffWithVerts surf model image polyRef idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        texInfo <- readRef (surf^.msTexInfo)
        let vec = readVertexPosition surf model idx
            s = vec `dot` ((fst (texInfo^.mtiVecs))^._xyz) + ((fst (texInfo^.mtiVecs))^._w)
            s' = s / (fromIntegral (image^.iWidth))
            t = vec `dot` ((snd (texInfo^.mtiVecs))^._xyz) + ((snd (texInfo^.mtiVecs))^._w)
            t' = t / (fromIntegral (image^.iHeight))
        Polygon.setPolyX polyRef idx (vec^._x)
        Polygon.setPolyY polyRef idx (vec^._y)
        Polygon.setPolyZ polyRef idx (vec^._z)
        Polygon.setPolyS1 polyRef idx s'
        Polygon.setPolyT1 polyRef idx t'
        -- lightmap texture coordinates
        let a = s - fromIntegral (fst (surf^.msTextureMins))
            b = a + fromIntegral (surf^.msLightS) * 16
            c = b + 8
            d = c / fromIntegral (Constants.blockWidth * 16)
            a' = t - fromIntegral (snd (surf^.msTextureMins))
            b' = a' + fromIntegral (surf^.msLightT) * 16
            c' = b' + 8
            d' = c' / fromIntegral (Constants.blockHeight * 16)
        Polygon.setPolyS2 polyRef idx d
        Polygon.setPolyT2 polyRef idx d'
        doStuffWithVerts surf model image polyRef (idx + 1) maxIdx

readVertexPosition :: MSurfaceT -> ModelT -> Int -> V3 Float
readVertexPosition surf model idx
    | li > 0 =
        let edge = (model^.mEdges) V.! li
            vertex = (model^.mVertexes) V.! fromIntegral (edge^.meV._1)
        in vertex^.mvPosition
    | otherwise =
        let edge = (model^.mEdges) V.! (negate li)
            vertex = (model^.mVertexes) V.! fromIntegral (edge^.meV._2)
        in vertex^.mvPosition
  where
    li = (model^.mSurfEdges) UV.! (surf^.msFirstEdge + idx)

lmAllocBlock :: Int -> Int -> (Int, Int) -> Quake (Bool, (Int, Int))
lmAllocBlock w h pos = do
    allocated <- use (fastRenderAPIGlobals.frGLLms.lmsAllocated)
    proceedAlloc allocated w h (findSpot allocated w Constants.blockHeight 0 (Constants.blockWidth - w) pos)

findSpot :: UV.Vector Int -> Int -> Int -> Int -> Int -> (Int, Int) -> ((Int, Int), Int)
findSpot allocated w best i maxI pos
    | i >= maxI = (pos, best)
    | j == w = findSpot allocated w best2 (i + 1) maxI (i, best2)
    | otherwise = findSpot allocated w best (i + 1) maxI pos
  where
    (best2, j) = findBest2 allocated best 0 i 0 w

findBest2 :: UV.Vector Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
findBest2 allocated best best2 i j maxJ
    | j >= maxJ = (best2, j)
    | allocated UV.! (i + j) >= best = (best2, j)
    | allocated UV.! (i + j) > best2 =
        findBest2 allocated best (allocated UV.! (i + j)) i (j + 1) maxJ
    | otherwise =
        findBest2 allocated best best2 i (j + 1) maxJ

proceedAlloc :: UV.Vector Int -> Int -> Int -> ((Int, Int), Int) -> Quake (Bool, (Int, Int))
proceedAlloc allocated w h (pos, best)
    | best + h > Constants.blockHeight = return (False, pos)
    | otherwise = do
        fastRenderAPIGlobals.frGLLms.lmsAllocated .= allocated'
        return (True, pos)
  where
    allocated' = runST $ do
        allocatedMutable <- UV.unsafeThaw allocated
        mapM_ (\idx -> MUV.write allocatedMutable idx (best + h)) [0..w-1]
        UV.unsafeFreeze allocatedMutable

lmInitBlock :: Quake ()
lmInitBlock = fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate Constants.blockWidth 0

rDrawWorld :: Quake ()
rDrawWorld = do
    drawWorld <- fmap (^.cvValue) drawWorldCVar
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
    proceedDrawWorld drawWorld newRefDef worldModelRef

proceedDrawWorld :: Float -> RefDefT -> Maybe (Ref' ModelT) -> Quake ()
proceedDrawWorld _ _ Nothing =
    Com.fatalError "Surf.rDrawWorld worldModelRef is Nothing"
proceedDrawWorld drawWorld newRefDef (Just worldModelRef)
    | drawWorld == 0 || (newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel /= 0 = return ()
    | otherwise = do
        fastRenderAPIGlobals.frCurrentModel .= Just worldModelRef
        fastRenderAPIGlobals.frModelOrg .= (newRefDef^.rdViewOrg)
        fastRenderAPIGlobals.frCurrentEntity .= Just (NewEntity (newEntityT & eFrame .~ truncate ((newRefDef^.rdTime) * 2)))
        fastRenderAPIGlobals.frGLState.glsCurrentTextures .= (-1, -1)
        request (GL.glColor3f 1 1 1)
        Warp.clearSkyBox
        Image.glEnableMultiTexture True
        texture0 <- use (fastRenderAPIGlobals.frTexture0)
        Image.glSelectTexture texture0
        Image.glTexEnv GL.GL_REPLACE
        polygonBuffer <- request (use frPolygonBuffer)
        request $ io $
            MSV.unsafeWith polygonBuffer (GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral Constants.byteStride))
        Image.glSelectTexture =<< use (fastRenderAPIGlobals.frTexture1)
        request $ io $ do
            MSV.unsafeWith (MSV.drop (Constants.stride - 2) polygonBuffer) (GL.glTexCoordPointer 2 GL.GL_FLOAT (fromIntegral Constants.byteStride))
            GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY
        checkLightmap =<< fmap (^.cvValue) glLightMapCVar
        recursiveWorldNode worldModelRef (Ref 0) -- root node
        texture1 <- use (fastRenderAPIGlobals.frTexture1)
        request $ do
            GL.glClientActiveTextureARB (fromIntegral texture1)
            GL.glDisableClientState GL.GL_TEXTURE_COORD_ARRAY
        Image.glEnableMultiTexture False
        drawTextureChains
        Warp.drawSkyBox
        drawTriangleOutlines
  where
    checkLightmap lightmap
        | lightmap /= 0 = Image.glTexEnv GL.GL_REPLACE
        | otherwise = Image.glTexEnv GL.GL_MODULATE

recursiveWorldNode :: Ref' ModelT -> Ref' MNodeT -> Quake ()
recursiveWorldNode worldModelRef (Ref nodeIdx) = do
    worldModel <- readRef worldModelRef
    let node = (worldModel^.mNodes) V.! nodeIdx
    visFrameCount <- use (fastRenderAPIGlobals.frVisFrameCount)
    nothingToDo <- checkIfNothingToDo (node^.mnContents) (node^.mnVisFrame) visFrameCount (node^.mnMins) (node^.mnMaxs)
    unless nothingToDo $ do
        modelOrg <- use (fastRenderAPIGlobals.frModelOrg)
        plane <- readRef (node^.mnPlane)
        drawNodesAndLeafs worldModelRef worldModel modelOrg plane node

drawNodesAndLeafs :: Ref' ModelT -> ModelT -> V3 Float -> CPlaneT -> MNodeT -> Quake ()
drawNodesAndLeafs worldModelRef worldModel modelOrg plane node = do
    drawChild child1
    frameCount <- use (fastRenderAPIGlobals.frFrameCount)
    drawNodeStuff worldModelRef worldModel node sidebit frameCount 0 (node^.mnNumSurfaces)
    drawChild child2
  where
    dot'| (plane^.cpType) == Constants.planeX = (modelOrg^._x) - (plane^.cpDist)
        | (plane^.cpType) == Constants.planeY = (modelOrg^._y) - (plane^.cpDist)
        | (plane^.cpType) == Constants.planeZ = (modelOrg^._z) - (plane^.cpDist)
        | otherwise                           = modelOrg `dot` (plane^.cpNormal) - (plane^.cpDist)
    (side, sidebit)
        | dot' > 0  = (0, 0)
        | otherwise = (1, Constants.surfPlaneBack)
    (child1, child2)
        | side == 0 = (node^.mnChildren._1, node^.mnChildren._2)
        | otherwise = (node^.mnChildren._2, node^.mnChildren._1)
    drawChild (MNodeChildRef nodeRef) = recursiveWorldNode worldModelRef nodeRef
    drawChild (MLeafChildRef leafRef) = drawLeafStuff worldModel leafRef

drawLeafStuff :: ModelT -> Ref' MLeafT -> Quake ()
drawLeafStuff worldModel (Ref leafIdx) = do
    visFrameCount <- use (fastRenderAPIGlobals.frVisFrameCount)
    nothingToDo <- checkIfNothingToDo (leaf^.mlContents) (leaf^.mlVisFrame) visFrameCount (leaf^.mlMins) (leaf^.mlMaxs)
    unless nothingToDo $ do
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        unless (((newRefDef^.rdAreaBits) UV.! ((leaf^.mlArea) `shiftR` 3)) .&. (1 `shiftL` ((leaf^.mlArea) .&. 7)) == 0) $ do -- unless not visible
            frameCount <- use (fastRenderAPIGlobals.frFrameCount)
            V.imapM_ (updateSurfaceFrameCount frameCount) (worldModel^.mMarkSurfaces)
  where
    leaf = (worldModel^.mLeafs) V.! leafIdx
    updateSurfaceFrameCount frameCount i surfRef
        | i >= (leaf^.mlMarkIndex) + (leaf^.mlNumMarkSurfaces) =
            modifyRef surfRef (\v -> v & msVisFrame .~ frameCount)
        | otherwise =
            return ()

-- TODO: try doing everything related to worldModel^.mSurfaces via QuakeRef (see if it is possible to use loadModelRef)
drawNodeStuff :: Ref' ModelT -> ModelT -> MNodeT -> Int -> Int -> Int -> Int -> Quake ()
drawNodeStuff worldModelRef worldModel node sidebit frameCount idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = doDrawNodeStuff ((worldModel^.mSurfaces) V.! surfIdx)
  where
    surfIdx = (node^.mnFirstSurface) + idx
    doDrawNodeStuff surf
        | (surf^.msVisFrame) /= frameCount || ((surf^.msFlags) .&. Constants.surfPlaneBack /= sidebit) =
            drawNodeStuff worldModelRef worldModel node sidebit frameCount (idx + 1) maxIdx
        | otherwise = do
            texInfo <- readRef (surf^.msTexInfo)
            processTextureInfo surf texInfo
            drawNodeStuff worldModelRef worldModel node sidebit frameCount (idx + 1) maxIdx
    processTextureInfo surf texInfo
        | (texInfo^.mtiFlags) .&. Constants.surfSky /= 0 =
            Warp.rAddSkySurface (Ref surfIdx)
        | (texInfo^.mtiFlags) .&. (Constants.surfTrans33 .|. Constants.surfTrans66) /= 0 = do
            alphaSurfaces <- use (fastRenderAPIGlobals.frAlphaSurfaces)
            modifyRef worldModelRef (\v -> v & mSurfaces.ix surfIdx.msTextureChain .~ alphaSurfaces)
            fastRenderAPIGlobals.frAlphaSurfaces .= Just (Ref surfIdx)
        | (surf^.msFlags) .&. Constants.surfDrawTurb == 0 =
            glRenderLightmappedPoly (Ref surfIdx)
        | otherwise = do
            -- the polygon is visible, so add it to the texture sorted chain
            -- FIXME: this is a hack for animation
            imageRef <- rTextureAnimation =<< readRef (surf^.msTexInfo)
            image <- readRef imageRef
            modifyRef worldModelRef (\v -> v & mSurfaces.ix surfIdx.msTextureChain .~ image^.iTextureChain)
            modifyRef imageRef (\v -> v & iTextureChain .~ Just (Ref surfIdx))

checkIfNothingToDo :: Int -> Int -> Int -> V3 Float -> V3 Float -> Quake Bool
checkIfNothingToDo contents visFrame visFrameCount mins maxs
    | contents == Constants.contentsSolid = return True
    | visFrame /= visFrameCount = return True
    | otherwise = rCullBox mins maxs

-- TODO: WARNING: this is incorrect, relies on QuakeRef MSurfaceT which uses
--                loadModelRef to get the MSurfaceT but that reference IS NOT
--                set to world model, need to set it beforehand for all the methods
--                and refactor everything
drawTextureChains :: Quake ()
drawTextureChains = do
    -- TODO: c_visible_textures -- useless for us?
    numGLTextures <- use (fastRenderAPIGlobals.frNumGLTextures)
    mapM_ renderTextureChain (fmap Ref [0..numGLTextures-1])
    Image.glEnableMultiTexture False
    mapM_ renderTextureChain2 (fmap Ref [0..numGLTextures-1])
    Image.glTexEnv GL.GL_REPLACE
  where
    renderTextureChain imageRef = do
        image <- readRef imageRef
        unless ((image^.iRegistrationSequence) == 0 || isNothing (image^.iTextureChain)) $
            drawTextureChain (image^.iTextureChain)
    drawTextureChain Nothing = return ()
    drawTextureChain (Just surfRef) = do
        surf <- readRef surfRef
        when ((surf^.msFlags) .&. Constants.surfDrawTurb == 0) $
            rRenderBrushPoly surfRef
        drawTextureChain (surf^.msTextureChain)
    renderTextureChain2 imageRef = do
        image <- readRef imageRef
        unless ((image^.iRegistrationSequence) == 0 || isNothing (image^.iTextureChain)) $ do
            drawTextureChain2 (image^.iTextureChain)
            modifyRef imageRef (\v -> v & iTextureChain .~ Nothing)
    drawTextureChain2 Nothing = return ()
    drawTextureChain2 (Just surfRef) = do
        surf <- readRef surfRef
        when ((surf^.msFlags) .&. Constants.surfDrawTurb /= 0) $
            rRenderBrushPoly surfRef
        drawTextureChain2 (surf^.msTextureChain)

drawTriangleOutlines :: Quake ()
drawTriangleOutlines = do
    showTrisValue <- fmap (^.cvValue) glShowTrisCVar
    when (showTrisValue /= 0) $
        error "Surf.drawTriangleOutlines" -- TODO

rCullBox :: V3 Float -> V3 Float -> Quake Bool
rCullBox mins maxs =
    cullBox =<< fmap (^.cvValue) noCullCVar
  where
    cullBox noCull
        | noCull /= 0 = return False
        | otherwise = doCullBox =<< use (fastRenderAPIGlobals.frFrustum)
    doCullBox frustum
        | Math3D.boxOnPlaneSide mins maxs (frustum V.! 0) == 2 = return True
        | Math3D.boxOnPlaneSide mins maxs (frustum V.! 1) == 2 = return True
        | Math3D.boxOnPlaneSide mins maxs (frustum V.! 2) == 2 = return True
        | Math3D.boxOnPlaneSide mins maxs (frustum V.! 3) == 2 = return True
        | otherwise                                             = return False

rTextureAnimation :: MTexInfoT -> Quake (Ref' ImageT)
rTextureAnimation tex
    | isNothing (tex^.mtiNext) =
        maybe imageError return (tex^.mtiImage)
    | otherwise = do
        currentEntityRef <- use (fastRenderAPIGlobals.frCurrentEntity)
        currentEntity <- maybe entityError getCurrentEntity currentEntityRef
        findFrame tex ((currentEntity^.eFrame) `mod` (tex^.mtiNumFrames))
  where
    imageError = do
        Com.fatalError "Surf.rTextureAnimation tex^.mtiImage is Nothing"
        return (Ref (-1))
    entityError = do
        Com.fatalError "Surf.rTextureAnimation fastRenderAPIGlobals.frCurrentEntity is Nothing"
        return newEntityT
    getCurrentEntity (VEntityRef entityRef) = readRef entityRef
    getCurrentEntity (RDEntityRef entityRef) = readRef entityRef
    getCurrentEntity (NewEntity entity) = return entity
    findFrame tex 0 = maybe imageError return (tex^.mtiImage)
    findFrame tex c = do
        texRef <- maybe texError return (tex^.mtiNext)
        nextTex <- readRef texRef
        findFrame nextTex (c - 1)
    texError = do
        Com.fatalError "Surf.rTextureAnimation#findFrame tex^.mtiNext is Nothing"
        return (Ref (-1))

glRenderLightmappedPoly :: Ref' MSurfaceT -> Quake ()
glRenderLightmappedPoly = error "Surf.glRenderLightmappedPoly" -- TODO

rRenderBrushPoly :: Ref' MSurfaceT -> Quake ()
rRenderBrushPoly = error "Surf.rRenderBrushPoly" -- TODO

rDrawBrushModel :: Ref RefDefT EntityT -> Quake ()
rDrawBrushModel = error "Surf.rDrawBrushModel" -- TODO