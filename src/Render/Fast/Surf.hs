{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Surf where

import Control.Lens ((.=), (^.), zoom, use, preuse, ix, (+=), (%=), _1, _2)
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (toUpper)
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
