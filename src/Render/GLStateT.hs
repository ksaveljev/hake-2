{-# LANGUAGE TemplateHaskell #-}
module Render.GLStateT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

data GLStateT =
  GLStateT { _glsInverseIntensity        :: Float
           , _glsFullScreen              :: Bool
           , _glsPrevMode                :: Int
           , _glsD16To8Table             :: B.ByteString
           , _glsLightmapTextures        :: Int
           , _glsCurrentTextures         :: (Int, Int)
           , _glsCurrentTmu              :: Int
           , _glsCameraSeparation        :: Float
           , _glsStereoEnabled           :: Bool
           , _glsOriginalRedGammaTable   :: UV.Vector Word8
           , _glsOriginalGreenGammaTable :: UV.Vector Word8
           , _glsOriginalBlueGammaTable  :: UV.Vector Word8
           }

makeLenses ''GLStateT
