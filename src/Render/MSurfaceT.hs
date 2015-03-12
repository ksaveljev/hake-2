{-# LANGUAGE TemplateHaskell #-}
module Render.MSurfaceT where

import Data.Int (Int16)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.CPlaneT
import Render.GLPolyT
import Render.MTexInfoT

data MSurfaceT =
  MSurfaceT { _msVisFrame           :: Int
            , _msPlane              :: CPlaneT
            , _msFlags              :: Int
            , _msFirstEdge          :: Int
            , _msNumEdges           :: Int
            , _msTextureMins        :: (Int16, Int16)
            , _msExtents            :: (Int16, Int16)
            , _msLightS             :: Int
            , _msLightT             :: Int
            , _msDLightS            :: Int
            , _msDLightT            :: Int
            , _msPolys              :: GLPolyT
            , _msTextureChain       :: MSurfaceT
            , _msLightmapChain      :: MSurfaceT
            , _msTexInfo            :: MTexInfoT
            , _msDLightFrame        :: Int
            , _msDLightBits         :: Int
            , _msLightmapTextureNum :: Int
            , _msStyles             :: B.ByteString
            , _msCachedLight        :: UV.Vector Float
            , _msSamples            :: B.ByteString
            }

makeLenses ''MSurfaceT
