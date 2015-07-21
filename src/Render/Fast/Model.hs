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
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
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

modInit :: Quake ()
modInit = do
    -- init mod_known
    models <- io $ V.replicateM maxModKnown (newIORef newModelT)
    fastRenderAPIGlobals.frModKnown .= models
    fastRenderAPIGlobals.frModNoVis .= B.replicate (Constants.maxMapLeafs `div` 8) 0xFF

rBeginRegistration :: B.ByteString -> Quake ()
rBeginRegistration model = do
    resetModelArrays
    Polygon.reset

    fastRenderAPIGlobals.frRegistrationSequence += 1
    fastRenderAPIGlobals.frOldViewCluster .= (-1) -- force markleafs

    let fullName = "maps/" `B.append` model `B.append` ".bsp"

    -- explicitly free the old map if different
    -- this guarantees that mod_known[0] is the world map
    Just flushMap <- CVar.get "flushmap" "0" 0
    Just modelRef <- preuse $ fastRenderAPIGlobals.frModKnown.ix 0
    model <- io $ readIORef modelRef
    let currentName = model^.mName

    when (currentName /= fullName || (flushMap^.cvValue) /= 0) $
      modFree modelRef

    modelRef <- modForName fullName True
    fastRenderAPIGlobals.frWorldModel .= modelRef

    fastRenderAPIGlobals.frViewCluster .= (-1)

modFree :: IORef ModelT -> Quake ()
modFree modelRef = io $ writeIORef modelRef newModelT

resetModelArrays :: Quake ()
resetModelArrays = do
    zoom (fastRenderAPIGlobals) $ do
      frModelTextureCoordIdx .= 0
      frModelVertexIndexIdx  .= 0
