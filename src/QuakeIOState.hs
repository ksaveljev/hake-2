{-# LANGUAGE TemplateHaskell #-}
module QuakeIOState where

import           Client.CDLightT (newCDLightT)
import           Client.CParticleT (newCParticleT)
import qualified Constants
import           Game.CPlaneT (newCPlaneT)
import           Game.EdictT (newEdictT)
import           QCommon.CBrushT (newCBrushT)
import           Render.ImageT
import           Render.ModelT
import           Render.MSurfaceT
import           Types

import           Control.Lens (makeLenses)
import           Data.IORef (newIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import           System.IO.Unsafe (unsafePerformIO)

makeLenses ''QuakeIOState

maxModKnown :: Int
maxModKnown = 512

modelBufferSize :: Int
modelBufferSize = 50000

stride :: Int
stride = 7

maxPolys :: Int
maxPolys = 20000

maxBufferVertices :: Int
maxBufferVertices = 120000

blockWidth :: Int
blockWidth = 128

blockHeight :: Int
blockHeight = 128

floodFillFifoSize :: Int
floodFillFifoSize = 0x1000

initialQuakeIOState :: QuakeIOState
initialQuakeIOState =
  QuakeIOState { _gCurTime               = unsafePerformIO (newIORef 0)
               , _gbGEdicts              = unsafePerformIO (V.thaw (V.generate (Constants.maxEdicts + 1) newEdictT)) -- one extra for "dummy edict"
               , _cmMapPlanes            = unsafePerformIO (V.thaw (V.replicate (Constants.maxMapPlanes + 6) newCPlaneT))
               , _cmMapBrushes           = unsafePerformIO (V.thaw (V.replicate Constants.maxMapBrushes newCBrushT))
               , _cText                  = unsafePerformIO (MSV.replicate Constants.conTextSize ' ')
               , _frGLTextures           = unsafePerformIO (V.thaw (V.generate Constants.maxGLTextures newImageT))
               , _frModKnown             = unsafePerformIO (V.thaw (V.replicate maxModKnown newModelT))
               , _frModInline            = unsafePerformIO (V.thaw (V.replicate maxModKnown newModelT))
               , _frFrustum              = unsafePerformIO (V.thaw (V.replicate 4 newCPlaneT))
               , _frVertexArrayBuf       = unsafePerformIO (MSV.new (Constants.maxVerts * 3))
               , _frModelTextureCoordBuf = unsafePerformIO (MSV.new (modelBufferSize * 2))
               , _frModelVertexIndexBuf  = unsafePerformIO (MSV.new modelBufferSize)
               , _frPolygonBuffer        = unsafePerformIO (MSV.new (maxBufferVertices * stride))
               , _frPolygonCache         = unsafePerformIO (MV.new maxPolys)
               , _lmsLightmapSurfaces    = unsafePerformIO (V.thaw (V.replicate Constants.maxLightMaps newMSurfaceT))
               , _lmsLightmapBuffer      = unsafePerformIO (MSV.new (4 * blockWidth * blockHeight))
               , _cgDLights              = unsafePerformIO (V.thaw (V.replicate Constants.maxDLights newCDLightT))
               , _cgParticles            = unsafePerformIO (V.thaw (V.replicate Constants.maxParticles newCParticleT))
               , _pVertexArray           = unsafePerformIO (MSV.new (Constants.maxParticles * 3))
               , _pColorArray            = unsafePerformIO (MSV.new Constants.maxParticles)
               , _frFifo                 = unsafePerformIO (MV.replicate floodFillFifoSize (0, 0))
               , _cHNodes1               = Nothing
               }
