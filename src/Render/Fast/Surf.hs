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
import           Data.IORef                   (IORef, newIORef, modifyIORef', readIORef)
import           Data.Maybe                   (isNothing)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV
import           Data.Word                    (Word8)
import           Foreign.Marshal.Array        (withArray)
import qualified Graphics.GL                  as GL
import           Linear                       (V3(..), dot, _x, _y, _z, _xyz, _w)
import           System.IO.Unsafe             (unsafePerformIO)

import           Client.EntityT
import           Client.LightStyleT
import           Client.RefDefT
import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import qualified QCommon.Com                  as Com
import           QCommon.CVarVariables
import           Render.Fast.GLLightMapStateT
import qualified Render.Fast.Image            as Image
import qualified Render.Fast.Light            as Light
import qualified Render.Fast.Mesh             as Mesh
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
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary                  (encode)
import qualified Util.Math3D                  as Math3D

-- TODO: Vector or ByteString?
dummy :: SV.Vector Word8
dummy = SV.replicate (4 * 128 * 128) 0

temp :: MSV.IOVector Word8
temp = unsafePerformIO (MSV.new (4 * 128 * 128))

lightmapBytes :: Int
lightmapBytes = 4

glLightmapFormat :: GL.GLenum
glLightmapFormat = GL.GL_RGBA

rDrawAlphaSurfaces :: Quake ()
rDrawAlphaSurfaces = do
    worldMatrix <- use (fastRenderAPIGlobals.frWorldMatrix)
    io $ withArray worldMatrix $ \ptr -> GL.glLoadMatrixf ptr
    GL.glEnable GL.GL_BLEND
    Image.glTexEnv GL.GL_MODULATE
    glState <- use (fastRenderAPIGlobals.frGLState)
    polygonBuffer <- use (fastRenderAPIGlobals.frPolygonBuffer)
    io $ MSV.unsafeWith polygonBuffer $ \ptr ->
        GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral Constants.byteStride) ptr
    alphaSurfaces <- use (fastRenderAPIGlobals.frAlphaSurfaces)
    drawSurfaces (realToFrac (glState^.glsInverseIntensity)) alphaSurfaces
    Image.glTexEnv GL.GL_REPLACE
    GL.glColor4f 1 1 1 1
    GL.glDisable GL.GL_BLEND
    fastRenderAPIGlobals.frAlphaSurfaces .= Nothing
  where
    drawSurfaces _ Nothing = return ()
    drawSurfaces intens (Just surfRef) = do
        surf <- io (readIORef surfRef)
        texInfo <- io (readIORef (surf^.msTexInfo))
        bindImage (texInfo^.mtiImage)
        fastRenderAPIGlobals.frCBrushPolys += 1
        setColors texInfo intens
        drawPolys surfRef surf texInfo
        drawSurfaces intens (surf^.msTextureChain)
    bindImage Nothing =
        Com.fatalError "Surf.rDrawAlphaSurfaces surf^.msTexInfo.mtiImage is Nothing"
    bindImage (Just imageRef) = do
        image <- readRef imageRef
        Image.glBind (image^.iTexNum)
    setColors texInfo intens
        | (texInfo^.mtiFlags) .&. Constants.surfTrans33 /= 0 =
            GL.glColor4f intens intens intens 0.33
        | (texInfo^.mtiFlags) .&. Constants.surfTrans66 /= 0 =
            GL.glColor4f intens intens intens 0.66
        | otherwise =
            GL.glColor4f intens intens intens 1
    drawPolys surfRef surf texInfo
        | (surf^.msFlags) .&. Constants.surfDrawTurb /= 0 =
            Warp.emitWaterPolys surfRef
        | (texInfo^.mtiFlags) .&. Constants.surfFlowing /= 0 =
            drawGLFlowingPoly (surf^.msPolys)
        | otherwise =
            drawGLPoly (surf^.msPolys)

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

proceedMarkLeaves :: Ref ModelT -> ModelT -> Float -> Int -> Int -> Quake ()
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
        io $ modifyIORef' ((worldModel^.mLeafs) V.! idx) (\v -> v & mlVisFrame .~ visFrameCount)
    setNodeVisFrame visFrameCount idx =
        io $ modifyIORef' ((worldModel^.mNodes) V.! idx) (\v -> v & mnVisFrame .~ visFrameCount)

combineClusters :: ModelT -> B.ByteString -> Int -> Quake B.ByteString
combineClusters worldModel vis viewCluster2 = do
    vis' <- Model.clusterPVS viewCluster2 worldModel
    return (B.pack (B.zipWith (\a b -> a .|. b) (B.take c fatvis) (B.take c vis')) `B.append` (B.drop c fatvis))
  where
    len = ((worldModel^.mNumLeafs) + 7) `shiftR` 3
    fatvis = B.take len vis `B.append` B.replicate ((Constants.maxMapLeafs `div` 8) - len) 0
    c = (((worldModel^.mNumLeafs) + 31) `shiftR` 5) `shiftL` 2

markLeaf :: Ref ModelT -> ModelT -> B.ByteString -> Int -> Int -> Quake ()
markLeaf worldModelRef worldModel vis visFrameCount idx = do
    leaf <- io (readIORef leafRef)
    doMarkLeaf leaf
  where
    leafRef = (worldModel^.mLeafs) V.! idx
    doMarkLeaf leaf
        | (leaf^.mlCluster) == -1 = return ()
        | (vis `B.index` ((leaf^.mlCluster) `shiftR` 3)) .&. (1 `shiftL` ((leaf^.mlCluster) .&. 7)) /= 0 = do
            when ((leaf^.mlVisFrame) /= visFrameCount) $ do
                io $ modifyIORef' leafRef (\v -> v & mlVisFrame .~ visFrameCount)
                markNode worldModelRef worldModel (leaf^.mlParent) visFrameCount
        | otherwise = return ()

-- TODO: verify that worldModel doesn't need to be refreshed upon each call
markNode :: Ref ModelT -> ModelT -> Maybe (IORef MNodeT) -> Int -> Quake ()
markNode _ _ Nothing _ = return ()
markNode worldModelRef worldModel (Just nodeRef) visFrameCount = do
    node <- io (readIORef nodeRef)
    doMarkNode node
  where
    doMarkNode node
        | node^.mnVisFrame == visFrameCount =
            return ()
        | otherwise = do
            io $ modifyIORef' nodeRef (\v -> v & mnVisFrame .~ visFrameCount)
            markNode worldModelRef worldModel (node^.mnParent) visFrameCount

glBeginBuildingLightmaps :: Ref ModelT -> Quake ()
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
    initializeDynamicLightmap lms
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
    io $ do
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
        GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
        doUploadBlock dynamic lms
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
        MSV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
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
        MSV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
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

glCreateSurfaceLightmap :: IORef MSurfaceT -> Quake ()
glCreateSurfaceLightmap surfaceRef = do
    surface <- io (readIORef surfaceRef)
    when ((surface^.msFlags) .&. (Constants.surfDrawSky .|. Constants.surfDrawTurb) == 0) $
        doCreateSurfaceLightmap surfaceRef surface

doCreateSurfaceLightmap :: IORef MSurfaceT -> MSurfaceT -> Quake ()
doCreateSurfaceLightmap surfaceRef surface = do
    pos <- tryAllocBlock
    lightmapTexture <- use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture)
    io $ modifyIORef' surfaceRef (\v -> v & msLightS .~ (pos^._1)
                                          & msLightT .~ (pos^._2)
                                          & msLightmapTextureNum .~ lightmapTexture)
    Light.rSetCacheState surfaceRef
    buffer <- use (fastRenderAPIGlobals.frGLLms.lmsLightmapBuffer)
    surface <- io (readIORef surfaceRef)
    Light.rBuildLightMap surface buffer (((pos^._2) * Constants.blockWidth + (pos^._1)) * lightmapBytes) (Constants.blockWidth * lightmapBytes)
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

glBuildPolygonFromSurface :: IORef MSurfaceT -> Quake ()
glBuildPolygonFromSurface surfRef = do
    surf <- io(readIORef surfRef)
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
        texInfo <- io (readIORef (surf^.msTexInfo))
        maybe texInfoImageError readRef (texInfo^.mtiImage)
    texInfoImageError = do
        Com.fatalError "Surf.glBuildPolygonFromSurface tex info image is Nothing"
        return (newImageT (-1))

buildPolygonFromSurface :: IORef MSurfaceT -> MSurfaceT -> ModelT -> ImageT -> Quake ()
buildPolygonFromSurface surfRef surf model image = do
    polyRef <- Polygon.create lNumVerts
    modifyRef polyRef (\v -> v & glpNext .~ (surf^.msPolys)
                               & glpFlags .~ (surf^.msFlags))
    io $ modifyIORef' surfRef (\v -> v & msPolys .~ Just polyRef)
    doStuffWithVerts surf model image polyRef 0 lNumVerts
  where
    lNumVerts = surf^.msNumEdges

doStuffWithVerts :: MSurfaceT -> ModelT -> ImageT -> Ref GLPolyT -> Int -> Int -> Quake ()
doStuffWithVerts surf model image polyRef idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        texInfo <- io (readIORef (surf^.msTexInfo))
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

proceedDrawWorld :: Float -> RefDefT -> Maybe (Ref ModelT) -> Quake ()
proceedDrawWorld _ _ Nothing =
    Com.fatalError "Surf.rDrawWorld worldModelRef is Nothing"
proceedDrawWorld drawWorld newRefDef (Just worldModelRef)
    | drawWorld == 0 || (newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel /= 0 = return ()
    | otherwise = do
        currentEntityRef <- io (newIORef (newEntityT & eFrame .~ truncate ((newRefDef^.rdTime) * 2)))
        fastRenderAPIGlobals.frCurrentModel .= Just worldModelRef
        fastRenderAPIGlobals.frModelOrg .= (newRefDef^.rdViewOrg)
        fastRenderAPIGlobals.frCurrentEntity .= Just currentEntityRef
        fastRenderAPIGlobals.frGLState.glsCurrentTextures .= (-1, -1)
        io (GL.glColor3f 1 1 1)
        Warp.clearSkyBox
        Image.glEnableMultiTexture True
        texture0 <- use (fastRenderAPIGlobals.frTexture0)
        Image.glSelectTexture texture0
        Image.glTexEnv GL.GL_REPLACE
        polygonBuffer <- use (fastRenderAPIGlobals.frPolygonBuffer)
        io (MSV.unsafeWith polygonBuffer (GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral Constants.byteStride)))
        Image.glSelectTexture =<< use (fastRenderAPIGlobals.frTexture1)
        io $ do
            MSV.unsafeWith (MSV.drop (Constants.stride - 2) polygonBuffer) (GL.glTexCoordPointer 2 GL.GL_FLOAT (fromIntegral Constants.byteStride))
            GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY
        checkLightmap =<< fmap (^.cvValue) glLightMapCVar
        worldModel <- readRef worldModelRef
        recursiveWorldNode worldModelRef ((worldModel^.mNodes) V.! 0) -- root node
        texture1 <- use (fastRenderAPIGlobals.frTexture1)
        io $ do
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

recursiveWorldNode :: Ref ModelT -> IORef MNodeT -> Quake ()
recursiveWorldNode worldModelRef nodeRef = do
    node <- io (readIORef nodeRef)
    visFrameCount <- use (fastRenderAPIGlobals.frVisFrameCount)
    nothingToDo <- checkIfNothingToDo (node^.mnContents) (node^.mnVisFrame) visFrameCount (node^.mnMins) (node^.mnMaxs)
    unless nothingToDo $ do
        modelOrg <- use (fastRenderAPIGlobals.frModelOrg)
        plane <- io (readIORef (node^.mnPlane))
        drawNodesAndLeafs worldModelRef modelOrg plane node

drawNodesAndLeafs :: Ref ModelT -> V3 Float -> CPlaneT -> MNodeT -> Quake ()
drawNodesAndLeafs worldModelRef modelOrg plane node = do
    worldModel <- readRef worldModelRef
    drawChild worldModel child1
    frameCount <- use (fastRenderAPIGlobals.frFrameCount)
    drawNodeStuff worldModelRef worldModel node sidebit frameCount 0 (node^.mnNumSurfaces)
    drawChild worldModel child2
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
    drawChild worldModel (MNodeChildRef nodeRef) = recursiveWorldNode worldModelRef nodeRef
    drawChild worldModel (MLeafChildRef leafRef) = drawLeafStuff worldModel leafRef

drawLeafStuff :: ModelT -> IORef MLeafT -> Quake ()
drawLeafStuff worldModel leafRef = do
    leaf <- io (readIORef leafRef)
    visFrameCount <- use (fastRenderAPIGlobals.frVisFrameCount)
    nothingToDo <- checkIfNothingToDo (leaf^.mlContents) (leaf^.mlVisFrame) visFrameCount (leaf^.mlMins) (leaf^.mlMaxs)
    unless nothingToDo $ do
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        unless (((newRefDef^.rdAreaBits) UV.! ((leaf^.mlArea) `shiftR` 3)) .&. (1 `shiftL` ((leaf^.mlArea) .&. 7)) == 0) $ do -- unless not visible
            frameCount <- use (fastRenderAPIGlobals.frFrameCount)
            io (V.imapM_ (updateSurfaceFrameCount leaf frameCount) (worldModel^.mMarkSurfaces))
  where
    updateSurfaceFrameCount leaf frameCount i surfRef
        | i >= (leaf^.mlMarkIndex) && i < (leaf^.mlMarkIndex) + (leaf^.mlNumMarkSurfaces) = do
            modifyIORef' surfRef (\v -> v & msVisFrame .~ frameCount)
        | otherwise = return ()

-- TODO: try doing everything related to worldModel^.mSurfaces via QuakeRef (see if it is possible to use loadModelRef)
drawNodeStuff :: Ref ModelT -> ModelT -> MNodeT -> Int -> Int -> Int -> Int -> Quake ()
drawNodeStuff worldModelRef@(Ref worldModelIdx) worldModel node sidebit frameCount idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        surf <- io (readIORef surfRef)
        doDrawNodeStuff surf
  where
    surfIdx = (node^.mnFirstSurface) + idx
    surfRef = (worldModel^.mSurfaces) V.! surfIdx
    doDrawNodeStuff surf
        | (surf^.msVisFrame) /= frameCount || ((surf^.msFlags) .&. Constants.surfPlaneBack /= sidebit) =
            drawNodeStuff worldModelRef worldModel node sidebit frameCount (idx + 1) maxIdx
        | otherwise = do
            texInfo <- io (readIORef (surf^.msTexInfo))
            processTextureInfo surf texInfo
            drawNodeStuff worldModelRef worldModel node sidebit frameCount (idx + 1) maxIdx
    processTextureInfo surf texInfo
        | (texInfo^.mtiFlags) .&. Constants.surfSky /= 0 =
            Warp.rAddSkySurface surfRef
        | (texInfo^.mtiFlags) .&. (Constants.surfTrans33 .|. Constants.surfTrans66) /= 0 = do
            alphaSurfaces <- use (fastRenderAPIGlobals.frAlphaSurfaces)
            io $ modifyIORef' surfRef (\v -> v & msTextureChain .~ alphaSurfaces)
            fastRenderAPIGlobals.frAlphaSurfaces .= Just surfRef
        | (surf^.msFlags) .&. Constants.surfDrawTurb == 0 =
            glRenderLightmappedPoly surfRef
        | otherwise = do
            -- the polygon is visible, so add it to the texture sorted chain
            -- FIXME: this is a hack for animation
            imageRef <- rTextureAnimation =<< io (readIORef (surf^.msTexInfo))
            image <- readRef imageRef
            io $ modifyIORef' surfRef (\v -> v & msTextureChain .~ image^.iTextureChain)
            modifyRef imageRef (\v -> v & iTextureChain .~ Just surfRef)

checkIfNothingToDo :: Int -> Int -> Int -> V3 Float -> V3 Float -> Quake Bool
checkIfNothingToDo contents visFrame visFrameCount mins maxs
    | contents == Constants.contentsSolid = return True
    | visFrame /= visFrameCount = return True
    | otherwise = rCullBox mins maxs

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
        surf <- io (readIORef surfRef)
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
        surf <- io (readIORef surfRef)
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

rTextureAnimation :: MTexInfoT -> Quake (Ref ImageT)
rTextureAnimation tex
    | isNothing (tex^.mtiNext) =
        maybe imageError return (tex^.mtiImage)
    | otherwise = do
        currentEntityRef <- maybe entityError return =<< use (fastRenderAPIGlobals.frCurrentEntity)
        currentEntity <- io (readIORef currentEntityRef)
        findFrame tex ((currentEntity^.eFrame) `mod` (tex^.mtiNumFrames))
  where
    imageError = do
        Com.fatalError "Surf.rTextureAnimation tex^.mtiImage is Nothing"
        return (Ref (-1))
    entityError = do
        Com.fatalError "Surf.rTextureAnimation fastRenderAPIGlobals.frCurrentEntity is Nothing"
        newEntityRef <- io (newIORef newEntityT)
        return newEntityRef
    findFrame tex 0 = maybe imageError return (tex^.mtiImage)
    findFrame tex c = do
        texRef <- maybe texError return (tex^.mtiNext)
        nextTex <- io (readIORef texRef)
        findFrame nextTex (c - 1)
    texError = do
        Com.fatalError "Surf.rTextureAnimation#findFrame tex^.mtiNext is Nothing"
        texInfoRef <- io (newIORef newMTexInfoT)
        return texInfoRef

glRenderLightmappedPoly :: IORef MSurfaceT -> Quake ()
glRenderLightmappedPoly surfRef = do
    surf <- io (readIORef surfRef)
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    frameCount <- use (fastRenderAPIGlobals.frFrameCount)
    -- DEBUG
    -- io $ print "SURF STYLES"
    -- io $ putStrLn $ concatMap (printf "0x%02X ") $ B.unpack (surf^.msStyles)
    -- io $ print "SURF CACHED LIGHT"
    -- io $ UV.mapM_ (\v -> print v) (surf^.msCachedLight)
    let (gotoDynamic, mapIdx) = calcGotoDynamic surf newRefDef 0 Constants.maxLightMaps
        mapIdx' = if mapIdx == 4 then 3 else mapIdx -- this is a hack from cwei
    -- DEBUG
    -- io $ print ("gotoDynamic = " ++ show gotoDynamic)
    -- io $ print ("map = " ++ show mapIdx')
    isDynamic <- checkIfDynamic surf gotoDynamic frameCount
    imageRef <- rTextureAnimation =<< io (readIORef (surf^.msTexInfo))
    -- DEBUG
    -- io $ print ("isDynamic = " ++ show isDynamic)
    image <- readRef imageRef
    doRenderLightmappedPoly surfRef surf image frameCount mapIdx' isDynamic
    texInfo <- io (readIORef (surf^.msTexInfo))
    doDrawPoly newRefDef surf texInfo
  where 
    calcGotoDynamic surf newRefDef idx maxIdx
        | idx >= maxIdx = (False, maxIdx)
        | (surf^.msStyles) `B.index` idx == 0xFF = (False, idx)
        | otherwise =
            let f = (surf^.msStyles) `B.index` idx
                white = ((newRefDef^.rdLightStyles) V.! (fromIntegral f))^.lsWhite
            in if white /= (surf^.msCachedLight) UV.! idx
                then (True, idx)
                else calcGotoDynamic surf newRefDef (idx + 1) maxIdx
    checkIfDynamic surf gotoDynamic frameCount
        | gotoDynamic || (surf^.msDLightFrame) == frameCount = do
            dynamic <- fmap (^.cvValue) glDynamicCVar
            doCheckIfDynamic surf dynamic
        | otherwise = return False
    doCheckIfDynamic surf dynamic
        | dynamic /= 0 = do
            texInfo <- io (readIORef (surf^.msTexInfo))
            return ((texInfo^.mtiFlags) .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) == 0)
        | otherwise = return False
    doDrawPoly newRefDef surf texInfo
        | (texInfo^.mtiFlags) .&. Constants.surfFlowing /= 0 = do
            let v = truncate ((newRefDef^.rdTime) / 40) :: Int
                scroll = (-64) * ( ((newRefDef^.rdTime) / 40) - fromIntegral v)
                scroll' = if scroll == 0 then -64 else scroll
            drawScrollingArrays (surf^.msPolys) scroll
        | otherwise = drawArrays (surf^.msPolys)
    drawArrays Nothing = return ()
    drawArrays (Just polyRef) = do
        poly <- readRef polyRef
        GL.glDrawArrays (fromIntegral (Constants.glPolygon))
                        (fromIntegral (poly^.glpPos))
                        (fromIntegral (poly^.glpNumVerts))
        drawArrays (poly^.glpChain)
    drawScrollingArrays Nothing _ = return ()
    drawScrollingArrays (Just polyRef) scroll = do
        poly <- readRef polyRef
        Polygon.beginScrolling poly scroll
        GL.glDrawArrays (fromIntegral (Constants.glPolygon))
                        (fromIntegral (poly^.glpPos))
                        (fromIntegral (poly^.glpNumVerts))
        Polygon.endScrolling poly

doRenderLightmappedPoly :: IORef MSurfaceT -> MSurfaceT -> ImageT -> Int -> Int -> Bool -> Quake ()
doRenderLightmappedPoly surfRef surf image frameCount mapIdx isDynamic
    | isDynamic = do
        lmtex <- calcLightMapTextureNum
        fastRenderAPIGlobals.frCBrushPolys += 1
        texture0 <- use (fastRenderAPIGlobals.frTexture0)
        texture1 <- use (fastRenderAPIGlobals.frTexture1)
        glState <- use (fastRenderAPIGlobals.frGLState)
        Image.glMBind texture0 (image^.iTexNum)
        Image.glMBind texture1 ((glState^.glsLightmapTextures) + lmtex)
    | otherwise = do
        fastRenderAPIGlobals.frCBrushPolys += 1
        texture0 <- use (fastRenderAPIGlobals.frTexture0)
        texture1 <- use (fastRenderAPIGlobals.frTexture1)
        glState <- use (fastRenderAPIGlobals.frGLState)
        Image.glMBind texture0 (image^.iTexNum)
        Image.glMBind texture1 ((glState^.glsLightmapTextures) + (surf^.msLightmapTextureNum))
  where
    f = (surf^.msStyles) `B.index` mapIdx
    calcLightMapTextureNum
        | (f >= 32 || f == 0) && (surf^.msDLightFrame) /= frameCount = do
            let smax = fromIntegral (((surf^.msExtents._1) `shiftR` 4) + 1)
                tmax = fromIntegral (((surf^.msExtents._2) `shiftR` 4) + 1)
            Light.rBuildLightMap surf temp 0 (smax * 4)
            -- DEBUG
            -- io $ print "TEMPTEMPTEMP DYNAMIC"
            -- io $ print ("flags = " ++ show (surf^.msFlags) ++
            --             " fe = " ++ show (surf^.msFirstEdge) ++
            --             " nume = " ++ show (surf^.msNumEdges) ++
            --             " tex num = " ++ show (surf^.msLightmapTextureNum))
            -- io $ print ("smax = " ++ show smax ++ " tmax = " ++ show tmax)
            -- io $ print $ (concat . map (flip showHex "") . B.unpack) temp
            Light.rSetCacheState surfRef
            texture1 <- use (fastRenderAPIGlobals.frTexture1)
            glState <- use (fastRenderAPIGlobals.frGLState)
            Image.glMBind texture1 ((glState^.glsLightmapTextures) + (surf^.msLightmapTextureNum))
            io $ MSV.unsafeWith temp $ \ptr -> do
                GL.glTexSubImage2D GL.GL_TEXTURE_2D
                                   0
                                   (fromIntegral (surf^.msLightS))
                                   (fromIntegral (surf^.msLightT))
                                   (fromIntegral smax)
                                   (fromIntegral tmax)
                                   glLightmapFormat
                                   GL.GL_UNSIGNED_BYTE
                                   ptr
            return (surf^.msLightmapTextureNum)
        | otherwise = do
            let smax = fromIntegral (((surf^.msExtents._1) `shiftR` 4) + 1)
                tmax = fromIntegral (((surf^.msExtents._2) `shiftR` 4) + 1)
            Light.rBuildLightMap surf temp 0 (smax * 4)
            -- DEBUG
            -- io $ print "TEMPTEMPTEMP"
            -- io $ print ("smax = " ++ show smax ++ " tmax = " ++ show tmax)
            -- io $ print $ (concat . map (flip showHex "") . B.unpack) temp
            texture1 <- use (fastRenderAPIGlobals.frTexture1)
            glState <- use (fastRenderAPIGlobals.frGLState)
            Image.glMBind texture1 ((glState^.glsLightmapTextures) + 0)
            io $ MSV.unsafeWith temp $ \ptr -> do
                GL.glTexSubImage2D GL.GL_TEXTURE_2D
                                   0
                                   (fromIntegral (surf^.msLightS))
                                   (fromIntegral (surf^.msLightT))
                                   (fromIntegral smax)
                                   (fromIntegral tmax)
                                   glLightmapFormat
                                   GL.GL_UNSIGNED_BYTE
                                   ptr
            return 0
{-
    let lmtex = surf^.msLightmapTextureNum

    if isDynamic
      then do
        let f = (surf^.msStyles) `B.index` mapIdx'
        lmtex' <- if (f >= 32 || f == 0) && (surf^.msDLightFrame) /= frameCount
                    then do
                      let smax = fromIntegral $ ((surf^.msExtents._1) `shiftR` 4) + 1
                          tmax = fromIntegral $ ((surf^.msExtents._2) `shiftR` 4) + 1

                      Light.rBuildLightMap surf temp 0 (smax * 4)
                      -- io $ print "TEMPTEMPTEMP DYNAMIC"
                      -- io $ print ("flags = " ++ show (surf^.msFlags) ++
                      --             " fe = " ++ show (surf^.msFirstEdge) ++
                      --             " nume = " ++ show (surf^.msNumEdges) ++
                      --             " tex num = " ++ show (surf^.msLightmapTextureNum))
                      -- io $ print ("smax = " ++ show smax ++ " tmax = " ++ show tmax)
                      -- io $ print $ (concat . map (flip showHex "") . B.unpack) temp
                      Light.rSetCacheState surfRef

                      texture1 <- use $ fastRenderAPIGlobals.frTexture1
                      glState <- use $ fastRenderAPIGlobals.frGLState
                      Image.glMBind texture1 ((glState^.glsLightmapTextures) + (surf^.msLightmapTextureNum))

                      io $ MSV.unsafeWith temp $ \ptr -> do
                        GL.glTexSubImage2D GL.gl_TEXTURE_2D
                                           0
                                           (fromIntegral $ surf^.msLightS)
                                           (fromIntegral $ surf^.msLightT)
                                           (fromIntegral smax)
                                           (fromIntegral tmax)
                                           glLightmapFormat
                                           GL.gl_UNSIGNED_BYTE
                                           ptr

                      return (surf^.msLightmapTextureNum)
                    else do
                      let smax = fromIntegral $ ((surf^.msExtents._1) `shiftR` 4) + 1
                          tmax = fromIntegral $ ((surf^.msExtents._2) `shiftR` 4) + 1

                      Light.rBuildLightMap surf temp 0 (smax * 4)
                      -- io $ print "TEMPTEMPTEMP"
                      -- io $ print ("smax = " ++ show smax ++ " tmax = " ++ show tmax)
                      -- io $ print $ (concat . map (flip showHex "") . B.unpack) temp

                      texture1 <- use $ fastRenderAPIGlobals.frTexture1
                      glState <- use $ fastRenderAPIGlobals.frGLState
                      Image.glMBind texture1 ((glState^.glsLightmapTextures) + 0)

                      io $ MSV.unsafeWith temp $ \ptr -> do
                        GL.glTexSubImage2D GL.gl_TEXTURE_2D
                                           0
                                           (fromIntegral $ surf^.msLightS)
                                           (fromIntegral $ surf^.msLightT)
                                           (fromIntegral $ smax)
                                           (fromIntegral $ tmax)
                                           glLightmapFormat
                                           GL.gl_UNSIGNED_BYTE
                                           ptr
          
                      return 0

        fastRenderAPIGlobals.frCBrushPolys += 1

        texture0 <- use $ fastRenderAPIGlobals.frTexture0
        texture1 <- use $ fastRenderAPIGlobals.frTexture1
        glState <- use $ fastRenderAPIGlobals.frGLState

        Image.glMBind texture0 (image^.iTexNum)
        Image.glMBind texture1 ((glState^.glsLightmapTextures) + lmtex')

        if (surf^.msTexInfo.mtiFlags) .&. Constants.surfFlowing /= 0
          then do
            let v = truncate ((newRefDef^.rdTime) / 40) :: Int
                scroll = (-64) * ( ((newRefDef^.rdTime) / 40) - fromIntegral v)
                scroll' = if scroll == 0 then -64 else scroll

            drawScrollingArrays (surf^.msPolys) scroll

          else
            drawArrays (surf^.msPolys)

      else do
        fastRenderAPIGlobals.frCBrushPolys += 1

        texture0 <- use $ fastRenderAPIGlobals.frTexture0
        texture1 <- use $ fastRenderAPIGlobals.frTexture1
        glState <- use $ fastRenderAPIGlobals.frGLState

        Image.glMBind texture0 (image^.iTexNum)
        Image.glMBind texture1 ((glState^.glsLightmapTextures) + lmtex)

        if (surf^.msTexInfo.mtiFlags) .&. Constants.surfFlowing /= 0
          then do
            let v = truncate ((newRefDef^.rdTime) / 40) :: Int
                scroll = (-64) * ( ((newRefDef^.rdTime) / 40) - fromIntegral v)
                scroll' = if scroll == 0 then -64 else scroll

            drawScrollingArrays (surf^.msPolys) scroll

          else
            drawArrays (surf^.msPolys)
-}

rRenderBrushPoly :: IORef MSurfaceT -> Quake ()
rRenderBrushPoly = error "Surf.rRenderBrushPoly" -- TODO

rDrawBrushModel :: IORef EntityT -> Quake ()
rDrawBrushModel entRef = do
    currentModelRef <- use (fastRenderAPIGlobals.frCurrentModel)
    maybe currentModelError (drawBrushModel entRef) currentModelRef
  where
    currentModelError = Com.fatalError "Surf.rDrawBrushModel currentModelRef is Nothing"

drawBrushModel :: IORef EntityT -> Ref ModelT -> Quake ()
drawBrushModel entRef currentModelRef = do
    currentModel <- readRef currentModelRef
    unless ((currentModel^.mNumModelSurfaces) == 0) $ do
        fastRenderAPIGlobals.frCurrentEntity .= Just entRef
        fastRenderAPIGlobals.frGLState.glsCurrentTextures .= (-1, -1)
        e <- io (readIORef entRef)
        let (rotated, mins, maxs)
                | (e^.eAngles._x) /= 0 || (e^.eAngles._y) /= 0 || (e^.eAngles._z) /= 0 =
                    (True, fmap (subtract (currentModel^.mRadius)) (e^.eOrigin), fmap (+ (currentModel^.mRadius)) (e^.eOrigin))
                | otherwise =
                    (False, (e^.eOrigin) + (currentModel^.mMins), (e^.eOrigin) + (currentModel^.mMaxs))
        ok <- rCullBox mins maxs
        unless ok $ do
            io (GL.glColor3f 1 1 1)
            newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
            let modelOrg = (newRefDef^.rdViewOrg) - (e^.eOrigin)
                modelOrg'
                    | rotated =
                        let org = modelOrg
                            (forward, right, up) = Math3D.angleVectors (e^.eAngles) True True True
                        in V3 (org `dot` forward) (negate $ org `dot` right) (org `dot` up)
                    | otherwise = modelOrg
            fastRenderAPIGlobals.frModelOrg .= modelOrg'
            io (GL.glPushMatrix)
            Mesh.rRotateForEntity (e & eAngles .~ (let V3 a b c = (e^.eAngles) in V3 (-a) b (-c)))
            Image.glEnableMultiTexture True
            Image.glSelectTexture =<< use (fastRenderAPIGlobals.frTexture0)
            Image.glTexEnv GL.GL_REPLACE
            polygonBuffer <- use (fastRenderAPIGlobals.frPolygonBuffer)
            io $ MSV.unsafeWith polygonBuffer $ \ptr ->
                GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral Constants.byteStride) ptr
            Image.glSelectTexture =<< use (fastRenderAPIGlobals.frTexture1)
            Image.glTexEnv GL.GL_MODULATE
            io $ do
                MSV.unsafeWith (MSV.drop (stride - 2) polygonBuffer) $ \ptr ->
                    GL.glTexCoordPointer 2 GL.GL_FLOAT (fromIntegral Constants.byteStride) ptr
                GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY
            rDrawInlineBModel
            texture1 <- use (fastRenderAPIGlobals.frTexture1)
            io $ do
                GL.glClientActiveTextureARB (fromIntegral texture1)
                GL.glDisableClientState GL.GL_TEXTURE_COORD_ARRAY
            Image.glEnableMultiTexture False
            io (GL.glPopMatrix)

rDrawInlineBModel :: Quake ()
rDrawInlineBModel = do
    currentModelRef <- use (fastRenderAPIGlobals.frCurrentModel)
    maybe currentModelError drawInlineBModel currentModelRef
  where
    currentModelError = Com.fatalError "Surf.rDrawInlineBModel currentModelRef is Nothing"

drawInlineBModel :: Ref ModelT -> Quake ()
drawInlineBModel currentModelRef@(Ref modelIdx) = do
    currentModel <- readRef currentModelRef
    -- calculate dynamic lighting for bmodel
    flashBlend <- fmap (^.cvValue) glFlashBlendCVar
    when (flashBlend == 0) $ do
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        markLights currentModel newRefDef ((currentModel^.mNodes) V.! (currentModel^.mFirstNode))
    currentEntityRef <- maybe entityError return =<< use (fastRenderAPIGlobals.frCurrentEntity)
    currentEntity <- io (readIORef currentEntityRef)
    when ((currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $ do
        io $ do
            GL.glEnable GL.GL_BLEND
            GL.glColor4f 1 1 1 0.25
        Image.glTexEnv GL.GL_MODULATE
    -- draw texture
    mapM_ (drawTexture currentModel) [0..(currentModel^.mNumModelSurfaces)-1]
    when ((currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $ do
        io $ do
            GL.glDisable GL.GL_BLEND
            GL.glColor4f 1 1 1 1
        Image.glTexEnv GL.GL_REPLACE
  where
    markLights currentModel newRefDef firstNodeRef = do
        mapM_ (markLight currentModel (MNodeChildRef firstNodeRef) newRefDef) [0..(newRefDef^.rdNumDLights)-1]
    markLight currentModel nodeChild newRefDef idx =
        Light.rMarkLights currentModel ((newRefDef^.rdDLights) V.! idx) (1 `shiftL` idx) nodeChild
    entityError = do
        Com.fatalError "Surf.drawInlineBModel fastRenderAPIGlobals.frCurrentEntity is Nothing"
        newEntityRef <- io (newIORef newEntityT)
        return newEntityRef
    drawTexture currentModel idx = do
        let psurfRef = (currentModel^.mSurfaces) V.! ((currentModel^.mFirstModelSurface) + idx)
        psurf <- io (readIORef psurfRef)
        dot' <- getDot (psurf^.msPlane)
        -- draw the polygon
        when ((psurf^.msFlags) .&. Constants.surfPlaneBack /= 0 && dot' < (negate Constants.backfaceEpsilon) || (psurf^.msFlags) .&. Constants.surfPlaneBack == 0 && dot' > Constants.backfaceEpsilon) $ do
            texInfo <- io (readIORef (psurf^.msTexInfo))
            doDrawTexture psurfRef psurf texInfo
    getDot Nothing = do
        Com.fatalError "Surf.drawInlineBModel psurf^.msPlane is Nothing"
        return 0
    getDot (Just pplaneRef) = do
        pplane <- io (readIORef pplaneRef)
        modelOrg <- use (fastRenderAPIGlobals.frModelOrg)
        return (modelOrg `dot` (pplane^.cpNormal) - (pplane^.cpDist))
    doDrawTexture psurfRef psurf texInfo
        | (texInfo^.mtiFlags) .&. (Constants.surfTrans33 .|. Constants.surfTrans66) /= 0 = do
            -- add to the translucent chain
            alphaSurfaces <- use (fastRenderAPIGlobals.frAlphaSurfaces)
            io $ modifyIORef' psurfRef (\v -> v & msTextureChain .~ alphaSurfaces)
            fastRenderAPIGlobals.frAlphaSurfaces .= Just psurfRef
        | (psurf^.msFlags) .&. Constants.surfDrawTurb == 0 =
            glRenderLightmappedPoly psurfRef
        | otherwise = do
            Image.glEnableMultiTexture False
            rRenderBrushPoly psurfRef
            Image.glEnableMultiTexture True

drawGLFlowingPoly :: Maybe (Ref GLPolyT) -> Quake ()
drawGLFlowingPoly = error "Surf.drawGLFlowingPoly" -- TODO

drawGLPoly :: Maybe (Ref GLPolyT) -> Quake ()
drawGLPoly Nothing = Com.fatalError "Surf.drawGLPoly polyRef is Nothing"
drawGLPoly (Just polyRef) = do
    poly <- readRef polyRef
    GL.glDrawArrays GL.GL_POLYGON (fromIntegral (poly^.glpPos)) (fromIntegral (poly^.glpNumVerts))
