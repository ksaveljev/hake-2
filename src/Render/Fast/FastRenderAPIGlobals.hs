{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.FastRenderAPIGlobals ( module Render.Fast.FastRenderAPIGlobals
                                        , module Client.EntityT
                                        , module Client.RefDefT
                                        , module Client.VidDefT
                                        , module Render.Fast.GLLightMapStateT
                                        , module Render.GLConfigT
                                        , module Render.GLPolyT
                                        , module Render.GLStateT
                                        , module Render.ImageT
                                        , module Render.ModelT
                                        ) where

import Control.Lens (makeLenses)
import Data.IORef (newIORef)
import Linear (V3(..), V4(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

import Internal
import Client.EntityT
import Client.RefDefT
import Client.VidDefT
import Render.Fast.GLLightMapStateT
import Render.GLConfigT
import Render.GLPolyT
import Render.GLStateT
import Render.ImageT
import Render.ModelT
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Render.OpenGL.QGLConstants as QGLConstants

makeLenses ''FastRenderAPIGlobals

maxModKnown :: Int
maxModKnown = 512

modelBufferSize :: Int
modelBufferSize = 50000

initialFastRenderAPIGlobals :: FastRenderAPIGlobals
initialFastRenderAPIGlobals =
  FastRenderAPIGlobals { _frGLDepthMin           = 0
                       , _frGLDepthMax           = 0
                       , _frGLConfig             = newGLConfigT
                       , _frGLState              = newGLStateT
                       , _frd8to24table          = UV.replicate 256 0
                       , _frVid                  = newVidDefT
                       , _frColorTableEXT        = False
                       , _frActiveTextureARB     = False
                       , _frPointParameterEXT    = False
                       , _frLockArraysEXT        = False
                       , _frSwapIntervalEXT      = False
                       , _frTexture0             = QGLConstants.glTexture0
                       , _frTexture1             = QGLConstants.glTexture1
                       , _frGLTexSolidFormat     = 3
                       , _frGLTexAlphaFormat     = 4
                       , _frGLFilterMin          = QGLConstants.glLinearMipmapNearest
                       , _frGLFilterMax          = QGLConstants.glLinear
                       , _frNumGLTextures        = 0
                       , _frGLTextures           = V.generate RenderAPIConstants.maxGLTextures (\idx -> unsafePerformIO $ newIORef (newImageT idx))
                       , _frLastModes            = (-1, -1)
                       , _frRegistrationSequence = 0
                       , _frGammaTable           = B.replicate 256 0
                       , _frIntensityTable       = B.replicate 256 0
                       , _frModKnown             = V.replicate maxModKnown (unsafePerformIO $ newIORef newModelT)
                       , _frModNumKnown          = 0
                       , _frLoadModel            = unsafePerformIO $ newIORef newModelT
                       , _frCurrentModel         = unsafePerformIO $ newIORef newModelT
                       , _frModInline            = V.replicate maxModKnown (unsafePerformIO $ newIORef newModelT)
                       , _frModNoVis             = ""
                       , _frNoTexture            = unsafePerformIO $ newIORef (newImageT (-1))
                       , _frParticleTexture      = unsafePerformIO $ newIORef (newImageT (-1))
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
                       , _frModelTextureCoordBuf = unsafePerformIO $ MSV.new (modelBufferSize * 2)
                       , _frModelVertexIndexBuf  = unsafePerformIO $ MSV.new modelBufferSize
                       , _frModelTextureCoordIdx = 0
                       , _frModelVertexIndexIdx  = 0
                       , _frPolygonS1Old         = UV.replicate maxVertices 0
                       , _frPolygonBuffer        = unsafePerformIO $ MSV.new (maxBufferVertices * stride)
                       , _frPolygonCache         = unsafePerformIO $ MV.new maxPolys
                       , _frPolygonBufferIndex   = 0
                       , _frPolygonCount         = 0
                       , _frGLLms                = newGLLightMapStateT
                       , _frNewRefDef            = newRefDefT
                       , _frFrameCount           = 0
                       , _frWarpFace             = Nothing
                       , _frModelVisibility      = Nothing
                       , _frSkyName              = ""
                       , _frSkyRotate            = 0
                       , _frSkyAxis              = V3 0 0 0
                       , _frSkyImages            = V.replicate 6 Nothing
                       , _frSkyMin               = 0
                       , _frSkyMax               = 0
                       , _frCBrushPolys          = 0
                       , _frCAliasPolys          = 0
                       , _frFrustum              = V.replicate 4 (unsafePerformIO $ newIORef newCPlaneT)
                       , _frDLightFrameCount     = 0
                       , _frOrigin               = V3 0 0 0
                       , _frVUp                  = V3 0 0 0
                       , _frVPn                  = V3 0 0 0
                       , _frVRight               = V3 0 0 0
                       , _frVBlend               = V4 0 0 0 0
                       , _frWorldMatrix          = replicate 16 0
                       , _frVisFrameCount        = 0
                       , _frModelOrg             = V3 0 0 0
                       , _frCurrentEntity        = newEntityT
                       , _frSkyMins              = (UV.empty, UV.empty)
                       , _frSkyMaxs              = (UV.empty, UV.empty)
                       , _frAlphaSurfaces        = Nothing
                       }
