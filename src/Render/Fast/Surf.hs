{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Surf where

import Control.Lens ((.=), (^.), zoom, use, preuse, ix, (+=), (%=), _1, _2)
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (toUpper)
import Data.IORef (newIORef, IORef, readIORef, modifyIORef')
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word8)
import Foreign.Marshal.Array (withArray)
import Linear (V3(..), dot, _w, _xyz, _x, _y, _z)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.GL as GL

import Numeric (showHex)
import Text.Printf (printf)

import Game.CVarT
import Client.LightStyleT
import Types
import QuakeState
import CVarVariables
import Client.LightStyleT
import qualified Constants
import qualified QCommon.Com as Com
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Light as Light
import qualified Render.Fast.Mesh as Mesh
import qualified Render.Fast.Model as Model
import qualified Render.Fast.Polygon as Polygon
import qualified Render.Fast.Warp as Warp
import qualified Render.OpenGL.QGLConstants as QGLConstants
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Math3D as Math3D

dummy :: B.ByteString
dummy = B.replicate (4 * 128 * 128) 0

temp :: MSV.IOVector Word8
temp = unsafePerformIO $ MSV.new (4 * 128 * 128)

glLightmapFormat :: GL.GLenum
glLightmapFormat = GL.GL_RGBA

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
    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
    io $ BU.unsafeUseAsCString dummy $ \ptr ->
      GL.glTexImage2D GL.GL_TEXTURE_2D
                      0
                      (fromIntegral (lms^.lmsInternalFormat))
                      (fromIntegral blockWidth)
                      (fromIntegral blockHeight)
                      0
                      (fromIntegral glLightmapFormat)
                      GL.GL_UNSIGNED_BYTE
                      ptr

glEndBuildingLightmaps :: Quake ()
glEndBuildingLightmaps = do
    lmUploadBlock False
    Image.glEnableMultiTexture False

glCreateSurfaceLightmap :: IORef MSurfaceT -> Quake ()
glCreateSurfaceLightmap surfRef = do
    surf <- io $ readIORef surfRef

    when ((surf^.msFlags) .&. (Constants.surfDrawSky .|. Constants.surfDrawTurb) == 0) $ do
      -- io $ print "CREATE SURFACE LIGHTMAP OK"

      let smax = fromIntegral $ ((surf^.msExtents._1) `shiftR` 4) + 1
          tmax = fromIntegral $ ((surf^.msExtents._2) `shiftR` 4) + 1

--      io $ print ("smax = " ++ show smax)
--      io $ print ("tmax = " ++ show tmax)
--      io $ print ("light_s = " ++ show (surf^.msLightS))
--      io $ print ("light_t = " ++ show (surf^.msLightT))

      (ok, pos) <- lmAllocBlock smax tmax (surf^.msLightS, surf^.msLightT)

      pos' <- if ok
                then return pos
                else do
                  -- io $ print "NOT OK"
                  lmUploadBlock False
                  lmInitBlock
                  (ok', pos') <- lmAllocBlock smax tmax (surf^.msLightS, surf^.msLightT)
                  unless ok' $
                    Com.comError Constants.errFatal ("Consecutive calls to LM_AllocBlock(" `B.append` BC.pack (show smax) `B.append` "," `B.append` BC.pack (show tmax) `B.append` ") failed\n") -- IMPROVE?
                  return pos'

      currentLightmapTexture <- use $ fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture
      io $ modifyIORef' surfRef (\v -> v { _msLightS = pos'^._1
                                         , _msLightT = pos'^._2
                                         , _msLightmapTextureNum = currentLightmapTexture
                                         })

      Light.rSetCacheState surfRef
      surf' <- io $ readIORef surfRef
      buffer <- use $ fastRenderAPIGlobals.frGLLms.lmsLightmapBuffer
      Light.rBuildLightMap surf' buffer (((surf'^.msLightT) * blockWidth + (surf'^.msLightS)) * lightmapBytes) (blockWidth * lightmapBytes)

glBuildPolygonFromSurface :: IORef MSurfaceT -> Quake ()
glBuildPolygonFromSurface surfRef = do
    surf <- io $ readIORef surfRef
    Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
    model <- io $ readIORef currentModelRef

    let Just imageRef = surf^.msTexInfo.mtiImage
        lNumVerts = surf^.msNumEdges

    image <- io $ readIORef imageRef
    polyRef@(GLPolyReference polyIdx) <- Polygon.create lNumVerts
    
    use (fastRenderAPIGlobals.frPolygonCache) >>= \polygonCache ->
      io $ do
        poly <- MV.read polygonCache polyIdx
        MV.write polygonCache polyIdx poly { _glpNext = surf^.msPolys, _glpFlags = surf^.msFlags }

    io $ modifyIORef' surfRef (\v -> v { _msPolys = Just polyRef })

    doStuffWithVerts surf model image polyRef 0 lNumVerts

  where doStuffWithVerts :: MSurfaceT -> ModelT -> ImageT -> GLPolyReference -> Int -> Int -> Quake ()
        doStuffWithVerts surf model image polyRef idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let li = (model^.mSurfEdges) V.! (surf^.msFirstEdge + idx)
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

              let s = vec `dot` ((fst $ surf^.msTexInfo.mtiVecs)^._xyz) + ((fst $ surf^.msTexInfo.mtiVecs)^._w)
                  s' = s / (fromIntegral $ image^.iWidth)

                  t = vec `dot` ((snd $ surf^.msTexInfo.mtiVecs)^._xyz) + ((snd $ surf^.msTexInfo.mtiVecs)^._w)
                  t' = t / (fromIntegral $ image^.iHeight)

              Polygon.setPolyX polyRef idx (vec^._x)
              Polygon.setPolyY polyRef idx (vec^._y)
              Polygon.setPolyZ polyRef idx (vec^._z)
    
              Polygon.setPolyS1 polyRef idx s'
              Polygon.setPolyT1 polyRef idx t'

              -- lightmap texture coordinates
              let a = s - fromIntegral (fst $ surf^.msTextureMins)
                  b = a + fromIntegral (surf^.msLightS) * 16
                  c = b + 8
                  d = c / fromIntegral (blockWidth * 16)

                  a' = t - fromIntegral (snd $ surf^.msTextureMins)
                  b' = a' + fromIntegral (surf^.msLightT) * 16
                  c' = b' + 8
                  d' = c' / fromIntegral (blockHeight * 16)

              Polygon.setPolyS2 polyRef idx d
              Polygon.setPolyT2 polyRef idx d'

              doStuffWithVerts surf model image polyRef (idx + 1) maxIdx

lmUploadBlock :: Bool -> Quake ()
lmUploadBlock dynamic = do
    lms <- use $ fastRenderAPIGlobals.frGLLms

    let texture = if dynamic
                    then 0
                    else lms^.lmsCurrentLightmapTexture

    use (fastRenderAPIGlobals.frGLState) >>= \glState ->
      Image.glBind ((glState^.glsLightmapTextures) + texture)

    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
    GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)

    if dynamic
      then do
        let h = UV.maximum (lms^.lmsAllocated)
            height = if h < 0 then 0 else h

        io $ MSV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
          GL.glTexSubImage2D GL.GL_TEXTURE_2D
                             0
                             0
                             0
                             (fromIntegral blockWidth)
                             (fromIntegral height)
                             glLightmapFormat
                             GL.GL_UNSIGNED_BYTE
                             ptr
      else do
        io $ MSV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
          GL.glTexImage2D GL.GL_TEXTURE_2D
                          0
                          (fromIntegral $ lms^.lmsInternalFormat)
                          (fromIntegral blockWidth)
                          (fromIntegral blockHeight)
                          0
                          glLightmapFormat
                          GL.GL_UNSIGNED_BYTE
                          ptr

        fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture += 1
        use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture) >>= \clt ->
          when (clt == 128) $ -- TODO: 128 should be a constant here but there is some name clashing
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
      -- if (GL_lockpvs.value != 0)
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
      fastRenderAPIGlobals.frCurrentModel .= Just worldModelRef

      fastRenderAPIGlobals.frModelOrg .= (newRefDef^.rdViewOrg)
      currentEntity <- io $ newIORef newEntityT { _eFrame = truncate ((newRefDef^.rdTime) * 2) }
      fastRenderAPIGlobals.frCurrentEntity .= Just currentEntity

      fastRenderAPIGlobals.frGLState.glsCurrentTextures .= (-1, -1)

      GL.glColor3f 1 1 1

      Warp.clearSkyBox

      Image.glEnableMultiTexture True

      use (fastRenderAPIGlobals.frTexture0) >>= Image.glSelectTexture
      Image.glTexEnv GL.GL_REPLACE

      polygonBuffer <- use $ fastRenderAPIGlobals.frPolygonBuffer
      io $ MSV.unsafeWith polygonBuffer $ \ptr ->
        GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral byteStride) ptr

      use (fastRenderAPIGlobals.frTexture1) >>= Image.glSelectTexture

      io $ MSV.unsafeWith (MSV.drop (stride - 2) polygonBuffer) $ \ptr ->
        GL.glTexCoordPointer 2 GL.GL_FLOAT (fromIntegral byteStride) ptr

      GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY

      lightmapValue <- liftM (^.cvValue) glLightMapCVar
      if lightmapValue /= 0
        then Image.glTexEnv GL.GL_REPLACE
        else Image.glTexEnv GL.GL_MODULATE

      worldModel <- io $ readIORef worldModelRef

      recursiveWorldNode ((worldModel^.mNodes) V.! 0) -- root node

      use (fastRenderAPIGlobals.frTexture1) >>= GL.glClientActiveTextureARB . fromIntegral
      GL.glDisableClientState GL.GL_TEXTURE_COORD_ARRAY

      Image.glEnableMultiTexture False

      drawTextureChains
      Warp.drawSkyBox
      drawTriangleOutlines

{-
- R_DrawAlphaSurfaces
- Draw water surfaces and windows.
- The BSP tree is waled front to back, so unwinding the chain
- of alpha_surfaces will draw back to front, giving proper ordering.
-}
rDrawAlphaSurfaces :: Quake ()
rDrawAlphaSurfaces = do
    worldMatrix <- use $ fastRenderAPIGlobals.frWorldMatrix

    io $ withArray worldMatrix $ \ptr -> GL.glLoadMatrixf ptr

    GL.glEnable GL.GL_BLEND
    Image.glTexEnv GL.GL_MODULATE

    glState <- use $ fastRenderAPIGlobals.frGLState
    let intens = glState^.glsInverseIntensity

    polygonBuffer <- use $ fastRenderAPIGlobals.frPolygonBuffer
    io $ MSV.unsafeWith polygonBuffer $ \ptr ->
      GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral byteStride) ptr

    alphaSurfaces <- use $ fastRenderAPIGlobals.frAlphaSurfaces
    drawSurfaces (realToFrac intens) alphaSurfaces

    Image.glTexEnv GL.GL_REPLACE
    GL.glColor4f 1 1 1 1
    GL.glDisable GL.GL_BLEND

    fastRenderAPIGlobals.frAlphaSurfaces .= Nothing

  where drawSurfaces :: GL.GLfloat -> Maybe (IORef MSurfaceT) -> Quake ()
        drawSurfaces _ Nothing = return ()
        drawSurfaces intens (Just surfRef) = do
          surf <- io $ readIORef surfRef
          image <- io $ readIORef (fromJust $ surf^.msTexInfo.mtiImage)
          Image.glBind (image^.iTexNum)

          fastRenderAPIGlobals.frCBrushPolys += 1

          if | (surf^.msTexInfo.mtiFlags) .&. Constants.surfTrans33 /= 0 ->
                 GL.glColor4f intens intens intens 0.33
             | (surf^.msTexInfo.mtiFlags) .&. Constants.surfTrans66 /= 0 ->
                 GL.glColor4f intens intens intens 0.66
             | otherwise ->
                 GL.glColor4f intens intens intens 1

          if | (surf^.msFlags) .&. Constants.surfDrawTurb /= 0 ->
                 Warp.emitWaterPolys surfRef
             | (surf^.msTexInfo.mtiFlags) .&. Constants.surfFlowing /= 0 ->
                 drawGLFlowingPoly (fromJust $ surf^.msPolys)
             | otherwise ->
                 drawGLPoly (fromJust $ surf^.msPolys)

          drawSurfaces intens (surf^.msTextureChain)

drawTextureChains :: Quake ()
drawTextureChains = do
    -- TODO: c_visible_textures -- useless for us?
    numGLTextures <- use $ fastRenderAPIGlobals.frNumGLTextures
    glTextures <- use $ fastRenderAPIGlobals.frGLTextures
    renderTextureChain glTextures 0 numGLTextures

    Image.glEnableMultiTexture False

    renderTextureChain2 glTextures 0 numGLTextures

    Image.glTexEnv GL.GL_REPLACE

  where renderTextureChain :: V.Vector (IORef ImageT) -> Int -> Int -> Quake ()
        renderTextureChain glTextures idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let imageRef = glTextures V.! idx
              image <- io $ readIORef imageRef

              if (image^.iRegistrationSequence) == 0 || isNothing (image^.iTextureChain)
                then
                  renderTextureChain glTextures (idx + 1) maxIdx
                else do
                  drawTextureChain (image^.iTextureChain)
                  renderTextureChain glTextures (idx + 1) maxIdx

        drawTextureChain :: Maybe (IORef MSurfaceT) -> Quake ()
        drawTextureChain Nothing = return ()
        drawTextureChain (Just surfRef) = do
          surf <- io $ readIORef surfRef
          when ((surf^.msFlags) .&. Constants.surfDrawTurb == 0) $
            rRenderBrushPoly surfRef
          drawTextureChain (surf^.msTextureChain)

        renderTextureChain2 :: V.Vector (IORef ImageT) -> Int -> Int -> Quake ()
        renderTextureChain2 glTextures idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let imageRef = glTextures V.! idx
              image <- io $ readIORef imageRef

              if (image^.iRegistrationSequence) == 0 || isNothing (image^.iTextureChain)
                then
                  renderTextureChain2 glTextures (idx + 1) maxIdx
                else do
                  drawTextureChain2 (image^.iTextureChain)
                  io $ modifyIORef' imageRef (\v -> v { _iTextureChain = Nothing })
                  renderTextureChain2 glTextures (idx + 1) maxIdx

        drawTextureChain2 :: Maybe (IORef MSurfaceT) -> Quake ()
        drawTextureChain2 Nothing = return ()
        drawTextureChain2 (Just surfRef) = do
          surf <- io $ readIORef surfRef
          when ((surf^.msFlags) .&. Constants.surfDrawTurb /= 0) $
            rRenderBrushPoly surfRef
          drawTextureChain2 (surf^.msTextureChain)

drawTriangleOutlines :: Quake ()
drawTriangleOutlines = do
    showTrisValue <- liftM (^.cvValue) glShowTrisCVar

    when (showTrisValue /= 0) $ do
      io (putStrLn "Surf.drawTriangleOutlines") >> undefined -- TODO

recursiveWorldNode :: IORef MNodeT -> Quake ()
recursiveWorldNode nodeRef = do
    node <- io $ readIORef nodeRef

    -- io $ print "recursiveWorldNode"
    -- io $ print ("num surfaces = " ++ show (node^.mnNumSurfaces))
    -- io $ print ("first surface = " ++ show (node^.mnFirstSurface))
    -- io $ print ("contents = " ++ show (node^.mnContents))
    -- io $ print ("visframe = " ++ show (node^.mnVisFrame))

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
             | otherwise -> do
                 ok <- rCullBox mins maxs
                 return ok

        drawLeafStuff :: ModelT -> IORef MLeafT -> Quake ()
        drawLeafStuff worldModel leafRef = do
          leaf <- io $ readIORef leafRef
          nothingToDo <- checkIfNothingToDo (leaf^.mlContents) (leaf^.mlVisFrame) (leaf^.mlMins) (leaf^.mlMaxs)

          unless nothingToDo $ do
            --io $ print "drawLeafStuff"

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
              -- io $ print "drawNodeStuff"
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
                           then do
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

        -- io $ print "CULL BOX!"
        -- io $ print mins
        -- io $ print maxs
        -- io $ V.mapM_ (\f -> do
        --                print (f^.cpNormal)
        --                print (f^.cpDist)
        --                print (f^.cpType)
        --                print (f^.cpSignBits)
        --                print (f^.cpPad)
        --              ) frustum'

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
        Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
        currentEntity <- io $ readIORef currentEntityRef
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

    -- io $ print "SURF STYLES"
    -- io $ putStrLn $ concatMap (printf "0x%02X ") $ B.unpack (surf^.msStyles)
    -- io $ print "SURF CACHED LIGHT"
    -- io $ UV.mapM_ (\v -> print v) (surf^.msCachedLight)

    let (gotoDynamic, mapIdx) = calcGotoDynamic surf newRefDef 0 Constants.maxLightMaps
        mapIdx' = if mapIdx == 4 then 3 else mapIdx -- this is a hack from cwei

    -- io $ print ("gotoDynamic = " ++ show gotoDynamic)
    -- io $ print ("map = " ++ show mapIdx')

    isDynamic <- checkIfDynamic surf gotoDynamic frameCount
    imageRef <- rTextureAnimation (surf^.msTexInfo)

    -- io $ print ("isDynamic = " ++ show isDynamic)

    image <- io $ readIORef imageRef
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
                        GL.glTexSubImage2D GL.GL_TEXTURE_2D
                                           0
                                           (fromIntegral $ surf^.msLightS)
                                           (fromIntegral $ surf^.msLightT)
                                           (fromIntegral smax)
                                           (fromIntegral tmax)
                                           glLightmapFormat
                                           GL.GL_UNSIGNED_BYTE
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
                        GL.glTexSubImage2D GL.GL_TEXTURE_2D
                                           0
                                           (fromIntegral $ surf^.msLightS)
                                           (fromIntegral $ surf^.msLightT)
                                           (fromIntegral $ smax)
                                           (fromIntegral $ tmax)
                                           glLightmapFormat
                                           GL.GL_UNSIGNED_BYTE
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

  where calcGotoDynamic :: MSurfaceT -> RefDefT -> Int -> Int -> (Bool, Int)
        calcGotoDynamic surf newRefDef idx maxIdx
          | idx >= maxIdx = (False, maxIdx)
          | (surf^.msStyles) `B.index` idx == 0xFF = (False, idx)
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

        -- TODO: from Quake () to IO () ?
        drawArrays :: Maybe GLPolyReference -> Quake ()
        drawArrays Nothing = return ()
        drawArrays (Just (GLPolyReference polyIdx)) = do
          polygonCache <- use $ fastRenderAPIGlobals.frPolygonCache
          poly <- io $ MV.read polygonCache polyIdx
          GL.glDrawArrays (fromIntegral $ QGLConstants.glPolygon)
                          (fromIntegral $ poly^.glpPos)
                          (fromIntegral $ poly^.glpNumVerts)
          drawArrays (poly^.glpChain)

        drawScrollingArrays :: Maybe GLPolyReference -> Float -> Quake ()
        drawScrollingArrays Nothing _ = return ()
        drawScrollingArrays (Just (GLPolyReference polyIdx)) scroll = do
          polygonCache <- use $ fastRenderAPIGlobals.frPolygonCache
          poly <- io $ MV.read polygonCache polyIdx
          Polygon.beginScrolling poly scroll
          GL.glDrawArrays (fromIntegral $ QGLConstants.glPolygon)
                          (fromIntegral $ poly^.glpPos)
                          (fromIntegral $ poly^.glpNumVerts)
          Polygon.endScrolling poly

lmAllocBlock :: Int -> Int -> (Int, Int) -> Quake (Bool, (Int, Int))
lmAllocBlock w h pos = do
    allocated <- use $ fastRenderAPIGlobals.frGLLms.lmsAllocated
    let (pos', best) = findSpot allocated blockHeight 0 (blockWidth - w) pos
    -- io $ print "ALLOC BLOCK"
    -- io $ print ("best = " ++ show best)
    -- io $ print ("pos = " ++ show pos')
    --io $ print ("allocated = " ++ show (UV.take 10 allocated))
    if best + h > blockHeight
      then return (False, pos')
      else do
        let updates = collectUpdates (pos'^._1) best 0 w []
        fastRenderAPIGlobals.frGLLms.lmsAllocated %= (UV.// updates)
        return (True, pos')

  where findSpot :: UV.Vector Int -> Int -> Int -> Int -> (Int, Int) -> ((Int, Int), Int)
        findSpot allocated best i maxI pos
          | i >= maxI = (pos, best)
          | otherwise =
              let (best2, j) = findBest2 allocated best 0 i 0 w
              in if j == w -- this is a valid spot
                   then findSpot allocated best2 (i + 1) maxI (i, best2)
                   else findSpot allocated best (i + 1) maxI pos

        findBest2 :: UV.Vector Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
        findBest2 allocated best best2 i j maxJ
          | j >= maxJ = (best2, j)
          | otherwise =
              let v = allocated UV.! (i + j)
              in if | v >= best -> (best2, j)
                    | v > best2 -> findBest2 allocated best v i (j + 1) maxJ
                    | otherwise -> findBest2 allocated best best2 i (j + 1) maxJ

        collectUpdates :: Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
        collectUpdates x best idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise = collectUpdates x best (idx + 1) maxIdx ((x + idx, best + h) : acc)

lmInitBlock :: Quake ()
lmInitBlock = fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate blockWidth 0

rRenderBrushPoly :: IORef MSurfaceT -> Quake ()
rRenderBrushPoly _ = do
    io (putStrLn "Surf.rRenderBrushPoly") >> undefined -- TODO

rDrawBrushModel :: IORef EntityT -> Quake ()
rDrawBrushModel entRef = do
    Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
    currentModel <- io $ readIORef currentModelRef

    unless ((currentModel^.mNumModelSurfaces) == 0) $ do
      fastRenderAPIGlobals.frCurrentEntity .= Just entRef
      fastRenderAPIGlobals.frGLState.glsCurrentTextures .= (-1, -1)

      e <- io $ readIORef entRef

      let (rotated, mins, maxs) = if (e^.eAngles._x) /= 0 || (e^.eAngles._y) /= 0 || (e^.eAngles._z) /= 0
                                    then (True, fmap (subtract (currentModel^.mRadius)) (e^.eOrigin), fmap (+ (currentModel^.mRadius)) (e^.eOrigin))
                                    else (False, (e^.eOrigin) + (currentModel^.mMins), (e^.eOrigin) + (currentModel^.mMaxs))

      ok <- rCullBox mins maxs

      unless ok $ do
        GL.glColor3f 1 1 1

        newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
        let modelOrg = (newRefDef^.rdViewOrg) - (e^.eOrigin)
            modelOrg' = if rotated
                          then let org = modelOrg
                                   (Just forward, Just right, Just up) = Math3D.angleVectors (e^.eAngles) True True True
                               in V3 (org `dot` forward) (negate $ org `dot` right) (org `dot` up)
                          else modelOrg

        fastRenderAPIGlobals.frModelOrg .= modelOrg'

        GL.glPushMatrix

        Mesh.rRotateForEntity e { _eAngles = let V3 a b c = (e^.eAngles) in V3 (-a) b (-c) }

        Image.glEnableMultiTexture True
        use (fastRenderAPIGlobals.frTexture0) >>= Image.glSelectTexture
        Image.glTexEnv GL.GL_REPLACE

        polygonBuffer <- use $ fastRenderAPIGlobals.frPolygonBuffer
        io $ MSV.unsafeWith polygonBuffer $ \ptr ->
          GL.glInterleavedArrays GL.GL_T2F_V3F (fromIntegral byteStride) ptr

        use (fastRenderAPIGlobals.frTexture1) >>= Image.glSelectTexture
        Image.glTexEnv GL.GL_MODULATE

        io $ MSV.unsafeWith (MSV.drop (stride - 2) polygonBuffer) $ \ptr ->
          GL.glTexCoordPointer 2 GL.GL_FLOAT (fromIntegral byteStride) ptr

        GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY

        rDrawInlineBModel

        use (fastRenderAPIGlobals.frTexture1) >>= \v -> GL.glClientActiveTextureARB (fromIntegral v)
        GL.glDisableClientState GL.GL_TEXTURE_COORD_ARRAY

        Image.glEnableMultiTexture False

        GL.glPopMatrix

rDrawInlineBModel :: Quake ()
rDrawInlineBModel = do
    Just currentModelRef <- use $ fastRenderAPIGlobals.frCurrentModel
    currentModel <- io $ readIORef currentModelRef

    -- calculate dynamic lighting for bmodel
    flashBlendValue <- liftM (^.cvValue) glFlashBlendCVar

    when (flashBlendValue == 0) $ do
      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
      let firstNodeRef = (currentModel^.mNodes) V.! (currentModel^.mFirstNode)
      markLights (MNodeChildReference firstNodeRef) newRefDef 0 (newRefDef^.rdNumDLights)

    Just currentEntityRef <- use $ fastRenderAPIGlobals.frCurrentEntity
    currentEntity <- io $ readIORef currentEntityRef
      
    when ((currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $ do
      GL.glEnable GL.GL_BLEND
      GL.glColor4f 1 1 1 0.25
      Image.glTexEnv GL.GL_MODULATE

    -- draw texture
    drawTexture currentModel 0 (currentModel^.mNumModelSurfaces)

    when ((currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $ do
      GL.glDisable GL.GL_BLEND
      GL.glColor4f 1 1 1 1
      Image.glTexEnv GL.GL_REPLACE

  where markLights :: MNodeChild -> RefDefT -> Int -> Int -> Quake ()
        markLights nodeChild newRefDef idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Light.rMarkLights ((newRefDef^.rdDLights) V.! idx) (1 `shiftL` idx) nodeChild
              markLights nodeChild newRefDef (idx + 1) maxIdx

        drawTexture :: ModelT -> Int -> Int -> Quake ()
        drawTexture currentModel idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let psurfRef = (currentModel^.mSurfaces) V.! ((currentModel^.mFirstModelSurface) + idx)
              psurf <- io $ readIORef psurfRef

              let Just pplaneRef = psurf^.msPlane
              pplane <- io $ readIORef pplaneRef

              modelOrg <- use $ fastRenderAPIGlobals.frModelOrg
              let dot' = modelOrg `dot` (pplane^.cpNormal) - (pplane^.cpDist)

              -- draw the polygon
              when ((psurf^.msFlags) .&. Constants.surfPlaneBack /= 0 && dot' < (negate RenderAPIConstants.backfaceEpsilon) || (psurf^.msFlags) .&. Constants.surfPlaneBack == 0 && dot' > RenderAPIConstants.backfaceEpsilon) $ do
                if | (psurf^.msTexInfo.mtiFlags) .&. (Constants.surfTrans33 .|. Constants.surfTrans66) /= 0 -> do
                       -- add to the translucent chain
                       alphaSurfaces <- use $ fastRenderAPIGlobals.frAlphaSurfaces
                       io $ modifyIORef' psurfRef (\v -> v { _msTextureChain = alphaSurfaces })
                       fastRenderAPIGlobals.frAlphaSurfaces .= Just psurfRef

                   | (psurf^.msFlags) .&. Constants.surfDrawTurb == 0 -> do
                       glRenderLightmappedPoly psurfRef

                   | otherwise -> do
                       Image.glEnableMultiTexture False
                       rRenderBrushPoly psurfRef
                       Image.glEnableMultiTexture True

              drawTexture currentModel (idx + 1) maxIdx

drawGLFlowingPoly :: GLPolyReference -> Quake ()
drawGLFlowingPoly _ = do
    io (putStrLn "Surf.drawGLFlowingPoly") >> undefined -- TODO

drawGLPoly :: GLPolyReference -> Quake ()
drawGLPoly (GLPolyReference polyIdx) = do
    polygonCache <- use $ fastRenderAPIGlobals.frPolygonCache
    poly <- io $ MV.read polygonCache polyIdx
    GL.glDrawArrays GL.GL_POLYGON (fromIntegral $ poly^.glpPos) (fromIntegral $ poly^.glpNumVerts)
