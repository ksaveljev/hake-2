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
import           Data.Bits (shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toUpper)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
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
     undefined -- TODO
{-
boolean LM_AllocBlock (int w, int h, pos_t pos)
	{
		int best = BLOCK_HEIGHT;
		int x = pos.x; 
		int best2;
		int i, j;
		for (i=0 ; i<BLOCK_WIDTH-w ; i++)
		{
			best2 = 0;

			for (j=0 ; j<w ; j++)
			{
				if (gl_lms.allocated[i+j] >= best)
					break;
				if (gl_lms.allocated[i+j] > best2)
					best2 = gl_lms.allocated[i+j];
			}
			if (j == w)
			{	// this is a valid spot
				pos.x = x = i;
				pos.y = best = best2;
			}
		}
		

		if (best + h > BLOCK_HEIGHT)
			return false;

		for (i=0 ; i<w ; i++)
			gl_lms.allocated[x + i] = best + h;

		return true;
	}
	-}

{-
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
          -}

lmInitBlock :: Quake ()
lmInitBlock = fastRenderAPIGlobals.frGLLms.lmsAllocated .= UV.replicate blockWidth 0