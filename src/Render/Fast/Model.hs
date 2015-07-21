{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Model where

import Control.Lens ((.=), (+=), preuse, ix, (^.), zoom, use, (%=), Traversal', _1, _2)
import Control.Monad (when, liftM)
import Data.Binary.IEEE754 (wordToFloat)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Int (Int8, Int32)
import Data.Maybe (isNothing, fromJust, isJust)
import Data.Monoid ((<>), mempty)
import Data.Word (Word8, Word16, Word32)
import Linear (V3(..), V4(..), norm, _x, _y, _z, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.QFiles.BSP.DFaceT
import QCommon.QFiles.BSP.DHeaderT
import QCommon.QFiles.BSP.DLeafT
import QCommon.QFiles.BSP.DModelT
import QCommon.QFiles.BSP.DNodeT
import QCommon.QFiles.BSP.DPlaneT
import QCommon.QFiles.MD2.DAliasFrameT
import QCommon.QFiles.MD2.DMdlT
import QCommon.QFiles.MD2.DSTVertT
import QCommon.QFiles.MD2.DTriangleT
import QCommon.QFiles.SP2.DSpriteT
import QCommon.TexInfoT
import QCommon.XCommandT
import Util.Binary
import qualified Constants
import qualified Client.VID as VID
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Polygon as Polygon
import {-# SOURCE #-} qualified Render.Fast.Surf as Surf
import qualified Render.Fast.Warp as Warp
import qualified Render.RenderAPIConstants as RenderAPIConstants
import qualified Util.Lib as Lib

modelListF :: XCommandT
modelListF = io (putStrLn "Model.modelListF") >> undefined -- TODO
