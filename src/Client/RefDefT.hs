{-# LANGUAGE TemplateHaskell #-}
module Client.RefDefT ( RefDefT(..)
                      , module Client.RefDefT
                      ) where

import Control.Lens (makeLenses)
import Linear (V3(..), V4(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Types

makeLenses ''RefDefT

newRefDefT :: RefDefT
newRefDefT =
  RefDefT { _rdX            = 0
          , _rdY            = 0
          , _rdWidth        = 0
          , _rdHeight       = 0
          , _rdFovX         = 0
          , _rdFovY         = 0
          , _rdViewOrg      = V3 0 0 0
          , _rdViewAngles   = V3 0 0 0
          , _rdBlend        = V4 0 0 0 0
          , _rdTime         = 0
          , _rdRdFlags      = 0
          , _rdAreaBits     = UV.empty
          , _rdLightStyles  = V.empty
          , _rdNumEntities  = 0
          , _rdEntities     = V.empty
          , _rdNumDLights   = 0
          , _rdDLights      = V.empty
          , _rdNumParticles = 0
          }
