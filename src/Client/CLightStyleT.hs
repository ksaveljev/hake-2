{-# LANGUAGE TemplateHaskell #-}
module Client.CLightStyleT
  ( module Client.CLightStyleT
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import           Linear (V3(..))
import qualified Data.Vector.Unboxed as UV

makeLenses ''CLightStyleT

newCLightStyleT :: CLightStyleT
newCLightStyleT =
  CLightStyleT { _clsLength = 0
               , _clsValue  = V3 0 0 0
               , _clsMap    = UV.replicate Constants.maxQPath 0
               }