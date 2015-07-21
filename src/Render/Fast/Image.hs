{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
module Render.Fast.Image where

import Control.Lens ((^.), (.=), (+=), use, preuse, ix, _1, _2, zoom, (%=))
import Control.Monad (when, void, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (toUpper)
import Data.Int (Int32)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.Monoid (mappend, mempty, mconcat)
import Data.Word (Word8)
import Foreign.Marshal.Utils (with)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import CVarVariables
import QCommon.MiptexT
import QCommon.QFiles.PcxT
import QCommon.QFiles.TgaT
import QCommon.XCommandT
import Render.GLModeT
import Render.GLTModeT
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.RenderAPIConstants as RenderAPIConstants

glSolidFormat :: Int
glSolidFormat = 3

glAlphaFormat :: Int
glAlphaFormat = 4

numGLModes :: Int
numGLModes = V.length modes

numGLAlphaModes :: Int
numGLAlphaModes = V.length glAlphaModes

numGLSolidModes :: Int
numGLSolidModes = V.length glSolidModes

-- must be a power of2
floodFillFifoSize :: Int
floodFillFifoSize = 0x1000

floodFillFifoMask :: Int
floodFillFifoMask = floodFillFifoSize - 1

modes :: V.Vector GLModeT
modes =
    V.fromList [ GLModeT "GL_NEAREST"                (fromIntegral GL.gl_NEAREST               ) (fromIntegral GL.gl_NEAREST)
               , GLModeT "GL_LINEAR"                 (fromIntegral GL.gl_LINEAR                ) (fromIntegral GL.gl_LINEAR )
               , GLModeT "GL_NEAREST_MIPMAP_NEAREST" (fromIntegral GL.gl_NEAREST_MIPMAP_NEAREST) (fromIntegral GL.gl_NEAREST)
               , GLModeT "GL_LINEAR_MIPMAP_NEAREST"  (fromIntegral GL.gl_LINEAR_MIPMAP_NEAREST ) (fromIntegral GL.gl_LINEAR )
               , GLModeT "GL_NEAREST_MIPMAP_LINEAR"  (fromIntegral GL.gl_NEAREST_MIPMAP_LINEAR ) (fromIntegral GL.gl_NEAREST)
               , GLModeT "GL_LINEAR_MIPMAP_LINEAR"   (fromIntegral GL.gl_LINEAR_MIPMAP_LINEAR  ) (fromIntegral GL.gl_LINEAR )
               ]

glAlphaModes :: V.Vector GLTModeT
glAlphaModes =
    V.fromList [ GLTModeT "default"    4
               , GLTModeT "GL_RGBA"    (fromIntegral GL.gl_RGBA   )
               , GLTModeT "GL_RGBA8"   (fromIntegral GL.gl_RGBA8  )
               , GLTModeT "GL_RGB5_A1" (fromIntegral GL.gl_RGB5_A1)
               , GLTModeT "GL_RGBA4"   (fromIntegral GL.gl_RGBA4  )
               , GLTModeT "GL_RGBA2"   (fromIntegral GL.gl_RGBA2  )
               ]

glSolidModes :: V.Vector GLTModeT
glSolidModes =
    V.fromList [ GLTModeT "default"     3
               , GLTModeT "GL_RGB"      (fromIntegral GL.gl_RGB     )
               , GLTModeT "GL_RGB8"     (fromIntegral GL.gl_RGB8    )
               , GLTModeT "GL_RGB5"     (fromIntegral GL.gl_RGB5    )
               , GLTModeT "GL_RGB4"     (fromIntegral GL.gl_RGB4    )
               , GLTModeT "GL_R3_G3_B2" (fromIntegral GL.gl_R3_G3_B2)
               ]
