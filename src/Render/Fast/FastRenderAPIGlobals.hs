{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.FastRenderAPIGlobals ( module Render.Fast.FastRenderAPIGlobals
                                        , module Client.VidDefT
                                        , module Render.Fast.GLLightMapStateT
                                        , module Render.GLConfigT
                                        , module Render.GLPolyT
                                        , module Render.GLStateT
                                        , module Render.ImageT
                                        , module Render.ModelT
                                        ) where

import Control.Lens (makeLenses)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Unboxed as UV

import Internal
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
                       , _frGLTextures           = V.generate RenderAPIConstants.maxGLTextures newImageT
                       , _frLastModes            = (-1, -1)
                       , _frRegistrationSequence = 0
                       , _frGammaTable           = B.replicate 256 0
                       , _frIntensityTable       = B.replicate 256 0
                       , _frModKnown             = V.replicate maxModKnown newModelT
                       , _frModNumKnown          = 0
                       , _frLoadModel            = ModKnownReference (-1)
                       , _frCurrentModel         = ModKnownReference (-1)
                       , _frModInline            = V.replicate maxModKnown newModelT
                       , _frModNoVis             = ""
                       , _frNoTexture            = ImageReference (-1)
                       , _frParticleTexture      = ImageReference (-1)
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
                       , _frModelTextureCoordBuf = unsafePerformIO $ MV.new (modelBufferSize * 2)
                       , _frModelVertexIndexBuf  = unsafePerformIO $ MV.new modelBufferSize
                       , _frModelTextureCoordIdx = 0
                       , _frModelVertexIndexIdx  = 0
                       , _frPolygonS1Old         = UV.replicate maxVertices 0
                       , _frPolygonBuffer        = unsafePerformIO $ MV.new (maxBufferVertices * stride)
                       , _frPolygonCache         = V.replicate maxPolys newGLPolyT
                       , _frPolygonBufferIndex   = 0
                       , _frPolygonCount         = 0
                       , _frGLLms                = newGLLightMapStateT
                       , _frNewRefDef            = newRefDefT
                       , _frFrameCount           = 0
                       , _frWarpFace             = Nothing
                       }
