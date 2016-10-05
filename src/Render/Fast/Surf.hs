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

import           Client.RefDefT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           Render.Fast.GLLightMapStateT
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Light as Light
import           Render.GLStateT
import           Render.MSurfaceT
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (^.), (.=), (%=), (&), (.~), _1, _2)
import           Control.Monad (when, unless)
import           Control.Monad.ST (runST)
import           Data.Bits (shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toUpper)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Data.Word (Word8)
import qualified Graphics.GL as GL
import           Linear (V3(..))

-- TODO: Vector or ByteString?
dummy :: SV.Vector Word8
dummy = SV.replicate (4 * 128 * 128) 0

lightmapBytes :: Int
lightmapBytes = 4

glLightmapFormat :: GL.GLenum
glLightmapFormat = GL.GL_RGBA

rDrawAlphaSurfaces :: Quake ()
rDrawAlphaSurfaces = error "Surf.rDrawAlphaSurfaces" -- TODO

rDrawWorld :: Quake ()
rDrawWorld = error "Surf.rDrawWorld" -- TODO

rMarkLeaves :: Quake ()
rMarkLeaves = error "Surf.rMarkLeaves" -- TODO

glBeginBuildingLightmaps :: Ref ModelT -> Quake ()
glBeginBuildingLightmaps _ =
  do fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate blockWidth 0
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
  where lightStyles = V.replicate Constants.maxLightStyles LightStyleT { _lsRGB = V3 1 1 1, _lsWhite = 3 }
        initializeDynamicLightmap lms =
          do GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR)
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
pickInternalFormat glMonoLightMap =
  do v <- checkFormat format
     fastRenderAPIGlobals.frGLLms.lmsInternalFormat .= v
  where format = toUpper (BC.index (glMonoLightMap^.cvString) 0)
        checkFormat 'A' = use (fastRenderAPIGlobals.frGLTexAlphaFormat)
        checkFormat 'C' = use (fastRenderAPIGlobals.frGLTexAlphaFormat)
        checkFormat 'I' = return Constants.glIntensity8
        checkFormat 'L' = return Constants.glLuminance8
        checkFormat _ = use (fastRenderAPIGlobals.frGLTexSolidFormat)

glEndBuildingLightmaps :: Quake ()
glEndBuildingLightmaps =
  do lmUploadBlock False
     Image.glEnableMultiTexture False

lmUploadBlock :: Bool -> Quake ()
lmUploadBlock = error "Surf.lmUploadBlock" -- TODO

glCreateSurfaceLightmap :: Ref MSurfaceT -> Quake ()
glCreateSurfaceLightmap surfaceRef =
  do surface <- readRef surfaceRef
     when ((surface^.msFlags) .&. (Constants.surfDrawSky .|. Constants.surfDrawTurb) == 0) $
       doCreateSurfaceLightmap surfaceRef surface

doCreateSurfaceLightmap :: Ref MSurfaceT -> MSurfaceT -> Quake ()
doCreateSurfaceLightmap surfaceRef surface =
  do pos <- tryAllocBlock
     lightmapTexture <- use (fastRenderAPIGlobals.frGLLms.lmsCurrentLightmapTexture)
     modifyRef surfaceRef (\v -> v & msLightS .~ (pos^._1)
                                   & msLightT .~ (pos^._2)
                                   & msLightmapTextureNum .~ lightmapTexture)
     Light.rSetCacheState surfaceRef
     buffer <- use (fastRenderAPIGlobals.frGLLms.lmsLightmapBuffer)
     Light.rBuildLightMap surfaceRef buffer (((pos^._2) * blockWidth + (pos^._1)) * lightmapBytes) (blockWidth * lightmapBytes)
  where smax = fromIntegral (((surface^.msExtents._1) `shiftR` 4) + 1)
        tmax = fromIntegral (((surface^.msExtents._2) `shiftR` 4) + 1)
        tryAllocBlock =
          do (ok, pos) <- lmAllocBlock smax tmax (surface^.msLightS, surface^.msLightT)
             retryAllocBlock ok pos
        retryAllocBlock True pos = return pos
        retryAllocBlock False _ =
          do lmUploadBlock False
             lmInitBlock
             (ok, pos) <- lmAllocBlock smax tmax (surface^.msLightS, surface^.msLightT)
             unless ok $
               Com.fatalError (B.concat ["Consecutive calls to LM_AllocBlock(", encode smax, ",", encode tmax, ") failed\n"])
             return pos

glBuildPolygonFromSurface :: Ref MSurfaceT -> Quake ()
glBuildPolygonFromSurface = error "Surf.glBuildPolygonFromSurface" -- TODO

lmAllocBlock :: Int -> Int -> (Int, Int) -> Quake (Bool, (Int, Int))
lmAllocBlock w h pos =
  do allocated <- use (fastRenderAPIGlobals.frGLLms.lmsAllocated)
     proceedAlloc allocated w h (findSpot allocated w blockHeight 0 (blockWidth - w) pos)

findSpot :: UV.Vector Int -> Int -> Int -> Int -> Int -> (Int, Int) -> ((Int, Int), Int)
findSpot allocated w best i maxI pos
  | i >= maxI = (pos, best)
  | j == w = findSpot allocated w best2 (i + 1) maxI (i, best2)
  | otherwise = findSpot allocated w best (i + 1) maxI pos
  where (best2, j) = findBest2 allocated best 0 i 0 w

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
  | otherwise =
      do fastRenderAPIGlobals.frGLLms.lmsAllocated .= allocated'
         return (True, pos)
  where allocated' = runST $
          do allocatedMutable <- UV.unsafeThaw allocated
             mapM_ (\idx -> MUV.write allocatedMutable idx (best + h)) [0..w-1]
             UV.unsafeFreeze allocatedMutable

lmInitBlock :: Quake ()
lmInitBlock = fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate blockWidth 0