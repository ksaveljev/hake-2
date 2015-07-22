{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Surf where

import Control.Lens ((.=), (^.), zoom, use, preuse, ix, (+=), (%=), _1, _2)
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (toUpper)
import Data.IORef (IORef, readIORef)
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

rMarkLeaves :: Quake ()
rMarkLeaves = do
    io (putStrLn "Surf.rMarkLeaves") >> undefined -- TODO

rDrawWorld :: Quake ()
rDrawWorld = do
    io (putStrLn "Surf.rDrawWorld") >> undefined -- TODO

rDrawAlphaSurfaces :: Quake ()
rDrawAlphaSurfaces = do
    io (putStrLn "Surf.rDrawAlphaSurfaces") >> undefined -- TODO
