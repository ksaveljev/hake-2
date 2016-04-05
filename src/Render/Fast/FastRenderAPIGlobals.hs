{-# LANGUAGE TemplateHaskell #-}
module Render.Fast.FastRenderAPIGlobals
  ( module Render.Fast.FastRenderAPIGlobals
  ) where

import           Client.RefDefT
import           Client.VidDefT
import qualified Constants
import           Game.CPlaneT (newCPlaneT)
import           Render.Fast.GLLightMapStateT
import           Render.GLConfigT
import           Render.GLStateT
import           Render.ImageT (newImageT)
import           Render.ModelT (newModelT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..), V4(..))

makeLenses ''FastRenderAPIGlobals

initialFastRenderAPIGlobals :: FastRenderAPIGlobals
initialFastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin           = 0
                       , _frGLDepthMax           = 0
                       , _frGLConfig             = newGLConfigT
                       , _frGLState              = newGLStateT
                       , _frd8to24table          = UV.replicate 256 0
                       , _frVid                  = newVidDefT
                       , _frGLTextures           = V.generate Constants.maxGLTextures newImageT
                       , _frModKnown             = V.replicate Constants.maxModKnown newModelT
                       , _frModInline            = V.replicate Constants.maxModKnown newModelT
                       , _frFrustum              = V.replicate 4 newCPlaneT
                       , _frColorTableEXT        = False
                       , _frActiveTextureARB     = False
                       , _frPointParameterEXT    = False
                       , _frLockArraysEXT        = False
                       , _frSwapIntervalEXT      = False
                       , _frTexture0             = Constants.glTexture0
                       , _frTexture1             = Constants.glTexture1
                       , _frGLTexSolidFormat     = 3
                       , _frGLTexAlphaFormat     = 4
                       , _frGLFilterMin          = Constants.glLinearMipmapNearest
                       , _frGLFilterMax          = Constants.glLinear
                       , _frNumGLTextures        = 0
                       , _frLastModes            = (-1, -1)
                       , _frRegistrationSequence = 0
                       , _frGammaTable           = B.replicate 256 0
                       , _frIntensityTable       = B.replicate 256 0
                       , _frModNumKnown          = 0
                       , _frCurrentModel         = Nothing
                       , _frModNoVis             = B.empty
                       , _frUploadWidth          = 0
                       , _frUploadHeight         = 0
                       , _frUploadedPaletted     = False
                       , _frDrawChars            = Nothing
                       , _frTrickFrame           = 0
                       , _frScrapDirty           = False
                       , _frViewCluster          = 0
                       , _frViewCluster2         = 0
                       , _frOldViewCluster       = 0
                       , _frOldViewCluster2      = 0
                       , _frWorldModel           = Nothing
                       , _frModelTextureCoordIdx = 0
                       , _frModelVertexIndexIdx  = 0
                       , _frPolygonS1Old         = UV.replicate Constants.maxVertices 0
                       , _frPolygonBufferIndex   = 0
                       , _frPolygonCount         = 0
                       , _frGLLms                = newGLLightMapStateT
                       , _frNewRefDef            = newRefDefT
                       , _frFrameCount           = 0
                       , _frWarpFace             = Nothing
                       , _frModelVisibility      = Nothing
                       , _frSkyName              = B.empty
                       , _frSkyRotate            = 0
                       , _frSkyAxis              = V3 0 0 0
                       , _frSkyImages            = V.replicate 6 Nothing
                       , _frSkyMin               = 0
                       , _frSkyMax               = 0
                       , _frCBrushPolys          = 0
                       , _frCAliasPolys          = 0
                       , _frDLightFrameCount     = 0
                       , _frOrigin               = V3 0 0 0
                       , _frVUp                  = V3 0 0 0
                       , _frVPn                  = V3 0 0 0
                       , _frVRight               = V3 0 0 0
                       , _frVBlend               = V4 0 0 0 0
                       , _frWorldMatrix          = replicate 16 0
                       , _frVisFrameCount        = 0
                       , _frModelOrg             = V3 0 0 0
                       , _frCurrentEntity        = Nothing
                       , _frSkyMins              = (UV.empty, UV.empty)
                       , _frSkyMaxs              = (UV.empty, UV.empty)
                       , _frAlphaSurfaces        = Nothing
                       , _frBlockLights          = UV.replicate (34 * 34 * 3) 0
                       , _frPointColor           = V3 0 0 0
                       , _frLightSpot            = V3 0 0 0
                       , _frRawPalette           = UV.replicate 0 256
                       , _frLoadModel            = Ref (-1)
                       , _frNoTexture            = Ref (-1)
                       , _frParticleTexture      = Ref (-1)
                       }