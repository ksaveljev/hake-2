{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Surf where

import Control.Lens ((.=), (^.), zoom, use, preuse, ix, (+=), (%=), _1, _2)
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (toUpper)
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (fromJust, isNothing)
import Linear (V3(..), dot, _w, _xyz, _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import CVarVariables
import Client.LightStyleT
import qualified Constants
import qualified QCommon.Com as Com
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Polygon as Polygon
import qualified Render.Fast.Warp as Warp
import qualified Render.OpenGL.QGLConstants as QGLConstants
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Math3D as Math3D

dummy :: B.ByteString
dummy = B.replicate (4 * 128 * 128) 0

glLightmapFormat :: GL.GLenum
glLightmapFormat = GL.gl_RGBA

glBeginBuildingLightmaps :: IORef ModelT -> Quake ()
glBeginBuildingLightmaps _ = do
    -- setup the base lightstyles so the lightmaps won't have to be
    -- regenerated the first time they're seen
    let lightStyles = V.replicate Constants.maxLightStyles LightStyleT { _lsRGB = V3 1 1 1, _lsWhite = 3 }

    zoom fastRenderAPIGlobals $ do
      frGLLms.lmsAllocated .= UV.replicate blockWidth 0
      frFrameCount .= 1 -- no dlightcache

    Image.glEnableMultiTexture True
    use (fastRenderAPIGlobals.frTexture1) >>= Image.glSelectTexture

    fastRenderAPIGlobals.frNewRefDef.rdLightStyles .= lightStyles

    use (fastRenderAPIGlobals.frGLState.glsLightmapTextures) >>= \lightmapTextures ->
      when (lightmapTextures == 0) $
        fastRenderAPIGlobals.frGLState.glsLightmapTextures .= RenderAPIConstants.texNumLightmaps

    fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture .= 1

    {-
    -- if mono lightmaps are enabled and we want to use alpha
    -- blending (a,1-a) then we're likely running on a 3DLabs
    -- Permedia2.  In a perfect world we'd use a GL_ALPHA lightmap
    -- in order to conserve space and maximize bandwidth, however 
    -- this isn't a perfect world.
    --
    -- So we have to use alpha lightmaps, but stored in GL_RGBA format,
    -- which means we only get 1/16th the color resolution we should when
    -- using alpha lightmaps.  If we find another board that supports
    -- only alpha lightmaps but that can at least support the GL_ALPHA
    -- format then we should change this code to use real alpha maps.
    -}
    glMonoLightMapCVar >>= \cvar -> do
      let format = toUpper (BC.index (cvar^.cvString) 0)
      v <- case format of
             'A' -> use (fastRenderAPIGlobals.frGLTexAlphaFormat)
             'C' -> use (fastRenderAPIGlobals.frGLTexAlphaFormat)
             'I' -> return QGLConstants.glIntensity8
             'L' -> return QGLConstants.glLuminance8
             _ -> use (fastRenderAPIGlobals.frGLTexSolidFormat)
      fastRenderAPIGlobals.frGLLms.lmsInternalFormat .= v

    -- initialize the dynamic lightmap texture
    glState <- use $ fastRenderAPIGlobals.frGLState
    lms <- use $ fastRenderAPIGlobals.frGLLms
    Image.glBind (glState^.glsLightmapTextures)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_LINEAR)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_LINEAR)
    io $ BU.unsafeUseAsCString dummy $ \ptr ->
      GL.glTexImage2D GL.gl_TEXTURE_2D
                      0
                      (fromIntegral (lms^.lmsInternalFormat))
                      (fromIntegral blockWidth)
                      (fromIntegral blockHeight)
                      0
                      (fromIntegral glLightmapFormat)
                      GL.gl_UNSIGNED_BYTE
                      ptr

glEndBuildingLightmaps :: Quake ()
glEndBuildingLightmaps = do
    lmUploadBlock False
    Image.glEnableMultiTexture False

glCreateSurfaceLightmap :: MSurfaceT -> Quake MSurfaceT
glCreateSurfaceLightmap surface = do
    if (surface^.msFlags) .&. (Constants.surfDrawSky .|. Constants.surfDrawTurb) /= 0
      then do
        io (putStrLn "Surf.glBuildPolygonFromSurface") >> undefined -- TODO
      else
        return surface

glBuildPolygonFromSurface :: MSurfaceT -> Quake MSurfaceT
glBuildPolygonFromSurface surface = do
    currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
    model <- io $ readIORef currentModelRef

    let Just imageRef = surface^.msTexInfo.mtiImage
        lNumVerts = surface^.msNumEdges

    image <- io $ readIORef imageRef
    polyRef@(GLPolyReference polyIdx) <- Polygon.create lNumVerts
    
    use (fastRenderAPIGlobals.frPolygonCache) >>= \polygonCache ->
      io $ do
        poly <- MV.read polygonCache polyIdx
        MV.write polygonCache polyIdx poly { _glpNext = surface^.msPolys, _glpFlags = surface^.msFlags }

    let surface' = surface { _msPolys = Just polyRef }

    doStuffWithVerts model image polyRef 0 lNumVerts

    return surface'

  where doStuffWithVerts :: ModelT -> ImageT -> GLPolyReference -> Int -> Int -> Quake ()
        doStuffWithVerts model image polyRef idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let li = (model^.mSurfEdges) V.! (surface^.msFirstEdge + idx)
              vec <- if li > 0
                       then do
                         let edgeRef = (model^.mEdges) V.! li
                         edge <- io $ readIORef edgeRef
                         let vertexRef = (model^.mVertexes) V.! (fromIntegral $ edge^.meV._1)
                         vertex <- io $ readIORef vertexRef
                         return (vertex^.mvPosition)
                       else do
                         let edgeRef = (model^.mEdges) V.! (negate li)
                         edge <- io $ readIORef edgeRef
                         let vertexRef = (model^.mVertexes) V.! (fromIntegral $ edge^.meV._2)
                         vertex <- io $ readIORef vertexRef
                         return (vertex^.mvPosition)

              let s = vec `dot` ((fst $ surface^.msTexInfo.mtiVecs)^._xyz) + ((fst $ surface^.msTexInfo.mtiVecs)^._w)
                  s' = s / (fromIntegral $ image^.iWidth)

                  t = vec `dot` ((snd $ surface^.msTexInfo.mtiVecs)^._xyz) + ((snd $ surface^.msTexInfo.mtiVecs)^._w)
                  t' = t / (fromIntegral $ image^.iHeight)

              Polygon.setPolyX polyRef idx (vec^._x)
              Polygon.setPolyY polyRef idx (vec^._y)
              Polygon.setPolyZ polyRef idx (vec^._z)
    
              Polygon.setPolyS1 polyRef idx s'
              Polygon.setPolyT1 polyRef idx t'

              -- lightmap texture coordinates
              let a = s - fromIntegral (fst $ surface^.msTextureMins)
                  b = a + fromIntegral (surface^.msLightS) * 16
                  c = b + 8
                  d = c / fromIntegral (blockWidth * 16)

                  a' = t - fromIntegral (snd $ surface^.msTextureMins)
                  b' = a' + fromIntegral (surface^.msLightT) * 16
                  c' = b' + 8
                  d' = c' / fromIntegral (blockHeight * 16)

              Polygon.setPolyS2 polyRef idx d
              Polygon.setPolyT2 polyRef idx d'

lmUploadBlock :: Bool -> Quake ()
lmUploadBlock dynamic = do
    lms <- use $ fastRenderAPIGlobals.frGLLms

    let texture = if dynamic
                    then 0
                    else lms^.lmsCurrentLightmapTexture

    use (fastRenderAPIGlobals.frGLState) >>= \glState ->
      Image.glBind ((glState^.glsLightmapTextures) + texture)

    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER (fromIntegral GL.gl_LINEAR)
    GL.glTexParameterf GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER (fromIntegral GL.gl_LINEAR)

    if dynamic
      then do
        let h = UV.maximum (lms^.lmsAllocated)
            height = if h < 0 then 0 else h

        io $ MSV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
          GL.glTexSubImage2D GL.gl_TEXTURE_2D
                             0
                             0
                             0
                             (fromIntegral blockWidth)
                             (fromIntegral height)
                             glLightmapFormat
                             GL.gl_UNSIGNED_BYTE
                             ptr
      else do
        io $ MSV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
          GL.glTexImage2D GL.gl_TEXTURE_2D
                          0
                          (fromIntegral $ lms^.lmsInternalFormat)
                          (fromIntegral blockWidth)
                          (fromIntegral blockHeight)
                          0
                          glLightmapFormat
                          GL.gl_UNSIGNED_BYTE
                          ptr

        fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture += 1
        use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture) >>= \clt ->
          when (clt == Constants.maxLightMaps) $
            Com.comError Constants.errDrop "LM_UploadBlock() - MAX_LIGHTMAPS exceeded\n"

{-
- R_MarkLeaves
- Mark the leaves and nodes that are in the PVS for
- the current cluster
-}
rMarkLeaves :: Quake ()
rMarkLeaves = do
    oldViewCluster <- use $ fastRenderAPIGlobals.frOldViewCluster
    oldViewCluster2 <- use $ fastRenderAPIGlobals.frOldViewCluster2
    viewCluster <- use $ fastRenderAPIGlobals.frViewCluster
    viewCluster2 <- use $ fastRenderAPIGlobals.frViewCluster2
    noVisValue <- liftM (^.cvValue) noVisCVar

    unless (oldViewCluster == viewCluster && oldViewCluster2 == viewCluster2 && noVisValue == 0 && viewCluster /= -1) $ do
      -- TODO: implement this development stuff
      -- if (gl_lockpvs.value != 0)
      --     return;
      
      zoom fastRenderAPIGlobals $ do
        frVisFrameCount += 1
        frOldViewCluster .= viewCluster
        frOldViewCluster2 .= viewCluster2

      Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      worldModel <- io $ readIORef worldModelRef

      if noVisValue /= 0 || viewCluster == -1 || isNothing (worldModel^.mVis)
        then do
          -- mark everything
          visFrameCount <- use $ fastRenderAPIGlobals.frVisFrameCount

          io $ markLeafs (worldModel^.mLeafs) visFrameCount 0 (worldModel^.mNumLeafs)
          io $ markNodes (worldModel^.mNodes) visFrameCount 0 (worldModel^.mNumNodes)

        else do
          vis <- Model.clusterPVS viewCluster worldModel

          -- may have to combine two clusters because of solid water boundaries
          vis' <- if viewCluster2 /= viewCluster
                    then combineClusters worldModel vis viewCluster2
                    else return vis

          visFrameCount <- use $ fastRenderAPIGlobals.frVisFrameCount
          markLeaf worldModelRef vis' visFrameCount 0 (worldModel^.mNumLeafs)

  where markLeafs :: V.Vector (IORef MLeafT) -> Int -> Int -> Int -> IO ()
        markLeafs leafs visFrameCount idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              modifyIORef' (leafs V.! idx) (\v -> v { _mlVisFrame = visFrameCount })
              markLeafs leafs visFrameCount (idx + 1) maxIdx

        markNodes :: V.Vector (IORef MNodeT) -> Int -> Int -> Int -> IO ()
        markNodes nodes visFrameCount idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              modifyIORef' (nodes V.! idx) (\v -> v { _mnVisFrame = visFrameCount })
              markNodes nodes visFrameCount (idx + 1) maxIdx

        combineClusters :: ModelT -> B.ByteString -> Int -> Quake B.ByteString
        combineClusters worldModel vis viewCluster2 = do
          let len = ((worldModel^.mNumLeafs) + 7) `shiftR` 3
              fatvis = B.take len vis `B.append` B.replicate ((Constants.maxMapLeafs `div` 8) - len) 0
              c = (((worldModel^.mNumLeafs) + 31) `shiftR` 5) `shiftL` 2

          vis' <- Model.clusterPVS viewCluster2 worldModel

          let fatvis' = B.pack (B.zipWith (\a b -> a .|. b) (B.take c fatvis) (B.take c vis')) `B.append` (B.drop c fatvis)
          return fatvis'

        markLeaf :: IORef ModelT -> B.ByteString -> Int -> Int -> Int -> Quake ()
        markLeaf worldModelRef vis visFrameCount idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              worldModel <- io $ readIORef worldModelRef
              let leafRef = (worldModel^.mLeafs) V.! idx
              leaf <- io $ readIORef leafRef
              let cluster = leaf^.mlCluster

              if cluster == -1
                then
                  markLeaf worldModelRef vis visFrameCount (idx + 1) maxIdx
                else do
                  when ((vis `B.index` (cluster `shiftR` 3)) .&. (1 `shiftL` (cluster .&. 7)) /= 0) $ do
                    when ((leaf^.mlVisFrame) /= visFrameCount) $ do
                      io $ modifyIORef' leafRef (\v -> v { _mlVisFrame = visFrameCount })
                      markNode (leaf^.mlParent) visFrameCount

                  markLeaf worldModelRef vis visFrameCount (idx + 1) maxIdx

        markNode :: Maybe (IORef MNodeT) -> Int -> Quake ()
        markNode Nothing _ = return ()
        markNode (Just nodeRef) visFrameCount = do
          node <- io $ readIORef nodeRef

          if (node^.mnVisFrame) == visFrameCount
            then
              return ()
            else do
              io $ modifyIORef' nodeRef (\v -> v { _mnVisFrame = visFrameCount })
              markNode (node^.mnParent) visFrameCount

rDrawWorld :: Quake ()
rDrawWorld = do
    drawWorldValue <- liftM (^.cvValue) drawWorldCVar
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

    unless (drawWorldValue == 0 || (newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel /= 0) $ do
      Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      fastRenderAPIGlobals.frCurrentModel .= worldModelRef

      fastRenderAPIGlobals.frModelOrg .= (newRefDef^.rdViewOrg)
      fastRenderAPIGlobals.frCurrentEntity .= newEntityT { _eFrame = truncate ((newRefDef^.rdTime) * 2) }

      fastRenderAPIGlobals.frGLState.glsCurrentTextures .= (-1, -1)

      GL.glColor3f 1 1 1

      Warp.clearSkyBox

      Image.glEnableMultiTexture True

      Image.glSelectTexture QGLConstants.glTexture0
      Image.glTexEnv GL.gl_REPLACE

      polygonBuffer <- use $ fastRenderAPIGlobals.frPolygonBuffer
      io $ MSV.unsafeWith polygonBuffer $ \ptr ->
        GL.glInterleavedArrays GL.gl_T2F_V3F (fromIntegral byteStride) ptr

      Image.glSelectTexture QGLConstants.glTexture1

      io $ MSV.unsafeWith (MSV.drop (stride - 2) polygonBuffer) $ \ptr ->
        GL.glTexCoordPointer 2 GL.gl_FLOAT (fromIntegral byteStride) ptr

      lightmapValue <- liftM (^.cvValue) glLightMapCVar
      if lightmapValue /= 0
        then Image.glTexEnv GL.gl_REPLACE
        else Image.glTexEnv GL.gl_MODULATE

      worldModel <- io $ readIORef worldModelRef

      recursiveWorldNode ((worldModel^.mNodes) V.! 0) -- root node

      GL.glClientActiveTextureARB (fromIntegral QGLConstants.glTexture1)
      GL.glDisableClientState GL.gl_TEXTURE_COORD_ARRAY

      Image.glEnableMultiTexture False

      drawTextureChains
      Warp.drawSkyBox
      drawTriangleOutlines

rDrawAlphaSurfaces :: Quake ()
rDrawAlphaSurfaces = do
    io (putStrLn "Surf.rDrawAlphaSurfaces") >> undefined -- TODO

drawTextureChains :: Quake ()
drawTextureChains = do
    io (putStrLn "Surf.drawTextureChains") >> undefined -- TODO

drawTriangleOutlines :: Quake ()
drawTriangleOutlines = do
    showTrisValue <- liftM (^.cvValue) glShowTrisCVar

    when (showTrisValue /= 0) $ do
      io (putStrLn "Surf.drawTriangleOutlines") >> undefined -- TODO

recursiveWorldNode :: IORef MNodeT -> Quake ()
recursiveWorldNode nodeRef = do
    node <- io $ readIORef nodeRef
    nothingToDo <- checkIfNothingToDo (node^.mnContents) (node^.mnVisFrame) (node^.mnMins) (node^.mnMaxs)

    unless nothingToDo $ do
      -- node is just a decision point, so go down the appropriate sides
      modelOrg <- use $ fastRenderAPIGlobals.frModelOrg
      plane <- io $ readIORef (node^.mnPlane)

      let dot' = if | (plane^.cpType) == Constants.planeX -> (modelOrg^._x) - (plane^.cpDist)
                    | (plane^.cpType) == Constants.planeY -> (modelOrg^._y) - (plane^.cpDist)
                    | (plane^.cpType) == Constants.planeZ -> (modelOrg^._z) - (plane^.cpDist)
                    | otherwise -> modelOrg `dot` (plane^.cpNormal) - (plane^.cpDist)

          (side, sidebit) = if dot' > 0
                              then (0, 0)
                              else (1, Constants.surfPlaneBack)

      Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      worldModel <- io $ readIORef worldModelRef

      let child = if side == 0 then node^.mnChildren._1 else node^.mnChildren._2

      case child of
        MNodeChildReference nodeRef -> recursiveWorldNode nodeRef
        MLeafChildReference leafRef -> drawLeafStuff worldModel leafRef

      frameCount <- use $ fastRenderAPIGlobals.frFrameCount
      drawNodeStuff worldModel node sidebit frameCount 0 (node^.mnNumSurfaces)

      -- recurse down the back side
      let child' = if side == 0 then node^.mnChildren._2 else node^.mnChildren._1

      case child' of
        MNodeChildReference nodeRef -> recursiveWorldNode nodeRef
        MLeafChildReference leafRef -> drawLeafStuff worldModel leafRef

  where checkIfNothingToDo :: Int -> Int -> V3 Float -> V3 Float -> Quake Bool
        checkIfNothingToDo contents visFrame mins maxs = do
          visFrameCount <- use $ fastRenderAPIGlobals.frVisFrameCount

          if | contents == Constants.contentsSolid -> return True
             | visFrame /= visFrameCount -> return True
             | otherwise -> rCullBox mins maxs

        drawLeafStuff :: ModelT -> IORef MLeafT -> Quake ()
        drawLeafStuff worldModel leafRef = do
          leaf <- io $ readIORef leafRef
          nothingToDo <- checkIfNothingToDo (leaf^.mlContents) (leaf^.mlVisFrame) (leaf^.mlMins) (leaf^.mlMaxs)

          unless nothingToDo $ do
            newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef

            let notVisible = ((newRefDef^.rdAreaBits) UV.! ((leaf^.mlArea) `shiftR` 3)) .&. (1 `shiftL` ((leaf^.mlArea) .&. 7)) == 0

            unless notVisible $ do
              let c = leaf^.mlNumMarkSurfaces
                  idx = leaf^.mlMarkIndex

              frameCount <- use $ fastRenderAPIGlobals.frFrameCount
              io $ V.imapM_ (\i s -> if i >= idx && i < idx + c then modifyIORef' s (\v -> v { _msVisFrame = frameCount }) else return ()) (worldModel^.mMarkSurfaces)

        drawNodeStuff :: ModelT -> MNodeT -> Int -> Int -> Int -> Int -> Quake ()
        drawNodeStuff worldModel node sidebit frameCount idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let surfRef = (worldModel^.mSurfaces) V.! ((node^.mnFirstSurface) + idx)
              surf <- io $ readIORef surfRef

              if (surf^.msVisFrame) /= frameCount || ((surf^.msFlags) .&. Constants.surfPlaneBack /= sidebit)
                then
                  drawNodeStuff worldModel node sidebit frameCount (idx + 1) maxIdx
                else do
                  let texInfo = surf^.msTexInfo

                  if | (texInfo^.mtiFlags) .&. Constants.surfSky /= 0 ->
                         Warp.rAddSkySurface surfRef
                     | (texInfo^.mtiFlags) .&. (Constants.surfTrans33 .|. Constants.surfTrans66) /= 0 -> do
                         alphaSurfaces <- use $ fastRenderAPIGlobals.frAlphaSurfaces
                         io $ modifyIORef' surfRef (\v -> v { _msTextureChain = alphaSurfaces })
                         fastRenderAPIGlobals.frAlphaSurfaces .= Just surfRef
                     | otherwise -> do
                         if (surf^.msFlags) .&. Constants.surfDrawTurb == 0
                           then
                             glRenderLightmappedPoly surfRef
                           else do
                             -- the polygon is visible, so add it to the
                             -- texture sorted chain
                             -- FIXME: this is a hack for animation
                             imageRef <- rTextureAnimation (surf^.msTexInfo)
                             image <- io $ readIORef imageRef
                             io $ modifyIORef' surfRef (\v -> v { _msTextureChain = image^.iTextureChain })
                             io $ modifyIORef' imageRef (\v -> v { _iTextureChain = Just surfRef })

                  drawNodeStuff worldModel node sidebit frameCount (idx + 1) maxIdx

rCullBox :: V3 Float -> V3 Float -> Quake Bool
rCullBox mins maxs = do
    noCullValue <- liftM (^.cvValue) noCullCVar

    if noCullValue /= 0
      then return False
      else do
        frustum <- use $ fastRenderAPIGlobals.frFrustum
        frustum' <- io $ V.mapM readIORef frustum
        if | Math3D.boxOnPlaneSide mins maxs (frustum' V.! 0) == 2 -> return True
           | Math3D.boxOnPlaneSide mins maxs (frustum' V.! 1) == 2 -> return True
           | Math3D.boxOnPlaneSide mins maxs (frustum' V.! 2) == 2 -> return True
           | Math3D.boxOnPlaneSide mins maxs (frustum' V.! 3) == 2 -> return True
           | otherwise -> return False

{-
- R_TextureAnimation
- Returns the proper texture for a given time and base texture
-}
rTextureAnimation :: MTexInfoT -> Quake (IORef ImageT)
rTextureAnimation tex = do
    case (tex^.mtiNext) of
      Nothing -> return (fromJust $ tex^.mtiImage)
      Just _ -> do
        currentEntity <- use $ fastRenderAPIGlobals.frCurrentEntity
        let c = (currentEntity^.eFrame) `mod` (tex^.mtiNumFrames)
        findFrame tex c

  where findFrame :: MTexInfoT -> Int -> Quake (IORef ImageT)
        findFrame tex 0 = return (fromJust $ tex^.mtiImage)
        findFrame tex c = do
          nextTex <- io $ readIORef (fromJust $ tex^.mtiNext)
          findFrame nextTex (c - 1)

glRenderLightmappedPoly :: IORef MSurfaceT -> Quake ()
glRenderLightmappedPoly surfRef = do
    surf <- io $ readIORef surfRef
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    frameCount <- use $ fastRenderAPIGlobals.frFrameCount

    let (gotoDynamic, mapIdx) = calcGotoDynamic surf newRefDef 0 Constants.maxLightMaps
        mapIdx' = if mapIdx == 4 then 3 else mapIdx -- this is a hack from cwei

    isDynamic <- checkIfDynamic surf gotoDynamic frameCount
    imageRef <- rTextureAnimation (surf^.msTexInfo)

    image <- io $ readIORef imageRef
    let lmtex = surf^.msLightmapTextureNum

    io (putStrLn "Surf.glRenderLightmappedPoly") >> undefined -- TODO

  where calcGotoDynamic :: MSurfaceT -> RefDefT -> Int -> Int -> (Bool, Int)
        calcGotoDynamic surf newRefDef idx maxIdx
          | idx >= maxIdx = (False, maxIdx)
          | otherwise =
              let f = (surf^.msStyles) `B.index` idx
                  white = ((newRefDef^.rdLightStyles) V.! (fromIntegral f))^.lsWhite
              in if white /= (surf^.msCachedLight) UV.! idx
                   then (True, idx)
                   else calcGotoDynamic surf newRefDef (idx + 1) maxIdx

        checkIfDynamic :: MSurfaceT -> Bool -> Int -> Quake Bool
        checkIfDynamic surf gotoDynamic frameCount =
          if gotoDynamic || (surf^.msDLightFrame) == frameCount
            then do
              dynamicValue <- liftM (^.cvValue) glDynamicCVar

              if dynamicValue /= 0
                then
                  return $ if (surf^.msTexInfo.mtiFlags) .&. (Constants.surfSky .|. Constants.surfTrans33 .|. Constants.surfTrans66 .|. Constants.surfWarp) == 0
                             then True
                             else False
                else
                  return False
            else
              return False
