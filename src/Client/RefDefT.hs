{-# LANGUAGE TemplateHaskell #-}
module Client.RefDefT where

import Data.Word (Word8)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Client.LightStyleT
import Client.EntityT
import Client.DLightT

data RefDefT =
  RefDefT { _rdX            :: Int
          , _rdY            :: Int
          , _rdWidth        :: Int
          , _rdHeight       :: Int
          , _rdFovX         :: Float
          , _rdFovY         :: Float
          , _rdViewOrg      :: V3 Float
          , _rdViewAngles   :: V3 Float
          , _rdBlend        :: V4 Float
          , _rdTime         :: Float
          , _rdRdFlags      :: Int
          , _rdAreaBits     :: UV.Vector Word8
          , _rdLightStyles  :: V.Vector LightStyleT
          , _rdNumEntities  :: Int
          , _rdEntities     :: V.Vector EntityT
          , _rdNumDLights   :: Int
          , _rdDLights      :: V.Vector DLightT
          , _rdNumParticles :: Int
          }

makeLenses ''RefDefT
