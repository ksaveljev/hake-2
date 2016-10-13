{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Surf
    ( glBeginBuildingLightmaps
    , glBuildPolygonFromSurface
    , glCreateSurfaceLightmap
    , glEndBuildingLightmaps
    , rDrawAlphaSurfaces
    , rDrawWorld
    , rMarkLeaves
    ) where

import           Control.Lens                 (use, (^.), (.=), (%=), (+=), (&), (.~))
import           Control.Lens                 (_1, _2)
import           Control.Monad                (when, unless)
import           Control.Monad.ST             (runST)
import           Data.Bits                    (shiftR, (.&.), (.|.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Char                    (toUpper)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV
import           Data.Word                    (Word8)
import qualified Graphics.GL                  as GL
import           Linear                       (V3(..), dot, _x, _y, _z, _xyz, _w)

import           Client.RefDefT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com                  as Com
import           QCommon.CVarVariables
import           Render.Fast.GLLightMapStateT
import qualified Render.Fast.Image            as Image
import qualified Render.Fast.Light            as Light
import qualified Render.Fast.Polygon          as Polygon
import           Render.GLPolyT
import           Render.GLStateT
import           Render.ImageT
import           Render.MEdgeT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Render.MVertexT
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary                  (encode)

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
rMarkLeaves = error "Surf.rMarkLeaves" -- TODO

glBeginBuildingLightmaps :: Ref ModelT -> Quake ()
glBeginBuildingLightmaps _ = do
    fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate blockWidth 0
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
                            (fromIntegral blockWidth)
                            (fromIntegral blockHeight)
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
                               (fromIntegral blockWidth)
                               (fromIntegral height)
                               glLightmapFormat
                               GL.GL_UNSIGNED_BYTE
                               ptr
    | otherwise =
        SV.unsafeWith (lms^.lmsLightmapBuffer) $ \ptr ->
            GL.glTexImage2D GL.GL_TEXTURE_2D
                            0
                            (fromIntegral (lms^.lmsInternalFormat))
                            (fromIntegral blockWidth)
                            (fromIntegral blockHeight)
                            0
                            glLightmapFormat
                            GL.GL_UNSIGNED_BYTE
                            ptr
  where
    h = UV.maximum (lms^.lmsAllocated)
    height = max 0 h

glCreateSurfaceLightmap :: Ref MSurfaceT -> Maybe B.ByteString -> Quake ()
glCreateSurfaceLightmap surfaceRef lightData = do
    surface <- readRef surfaceRef
    when ((surface^.msFlags) .&. (Constants.surfDrawSky .|. Constants.surfDrawTurb) == 0) $
        doCreateSurfaceLightmap surfaceRef surface lightData

doCreateSurfaceLightmap :: Ref MSurfaceT -> MSurfaceT -> Maybe B.ByteString -> Quake ()
doCreateSurfaceLightmap surfaceRef surface lightData = do
    pos <- tryAllocBlock
    lightmapTexture <- use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture)
    modifyRef surfaceRef (\v -> v & msLightS .~ (pos^._1)
                                  & msLightT .~ (pos^._2)
                                  & msLightmapTextureNum .~ lightmapTexture)
    Light.rSetCacheState surfaceRef
    buffer <- use (fastRenderAPIGlobals.frGLLms.lmsLightmapBuffer)
    Light.rBuildLightMap surfaceRef lightData buffer (((pos^._2) * blockWidth + (pos^._1)) * lightmapBytes) (blockWidth * lightmapBytes)
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

glBuildPolygonFromSurface :: Ref MSurfaceT -> Quake ()
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

buildPolygonFromSurface :: Ref MSurfaceT -> MSurfaceT -> ModelT -> ImageT -> Quake ()
buildPolygonFromSurface surfRef surf model image = do
    polyRef <- Polygon.create lNumVerts
    modifyRef polyRef (\v -> v & glpNext .~ (surf^.msPolys)
                               & glpFlags .~ (surf^.msFlags))
    modifyRef surfRef (\v -> v & msPolys .~ Just polyRef)
    doStuffWithVerts surf model image polyRef 0 lNumVerts
  where
    lNumVerts = surf^.msNumEdges

doStuffWithVerts :: MSurfaceT -> ModelT -> ImageT -> Ref GLPolyT -> Int -> Int -> Quake ()
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
            d = c / fromIntegral (blockWidth * 16)
            a' = t - fromIntegral (snd (surf^.msTextureMins))
            b' = a' + fromIntegral (surf^.msLightT) * 16
            c' = b' + 8
            d' = c' / fromIntegral (blockHeight * 16)
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
    proceedAlloc allocated w h (findSpot allocated w blockHeight 0 (blockWidth - w) pos)

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
    | best + h > blockHeight = return (False, pos)
    | otherwise = do
        fastRenderAPIGlobals.frGLLms.lmsAllocated .= allocated'
        return (True, pos)
  where
    allocated' = runST $ do
        allocatedMutable <- UV.unsafeThaw allocated
        mapM_ (\idx -> MUV.write allocatedMutable idx (best + h)) [0..w-1]
        UV.unsafeFreeze allocatedMutable

lmInitBlock :: Quake ()
lmInitBlock = fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate blockWidth 0

rDrawWorld :: Quake ()
rDrawWorld = do
    drawWorld <- fmap (^.cvValue) drawWorldCVar
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    proceedDrawWorld drawWorld newRefDef

proceedDrawWorld :: Float -> RefDefT -> Quake ()
proceedDrawWorld drawWorld newRefDef
    | drawWorld == 0 || (newRefDef^.rdRdFlags) .&. Constants.rdfNoWorldModel /= 0 = return ()
    | otherwise = do
        worldModelRef <- use (fastRenderAPIGlobals.frWorldModel)
        fastRenderAPIGlobals.frCurrentModel .= worldModelRef
        fastRenderAPIGlobals.frModelOrg .= (newRefDef^.rdViewOrg)
        error "Surf.rDrawWorld" -- TODO
