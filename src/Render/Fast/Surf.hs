{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Surf
  ( glBeginBuildingLightmaps
  , rDrawAlphaSurfaces
  , rDrawWorld
  , rMarkLeaves
  ) where

import           Client.RefDefT
import qualified Constants
import           Game.CVarT
import           QCommon.CVarVariables
import           Render.Fast.GLLightMapStateT
import qualified Render.Fast.Image as Image
import           Render.GLStateT
import           QuakeState
import           Types

import           Control.Lens (use, (^.), (.=), (%=))
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
