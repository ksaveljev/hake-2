{-# LANGUAGE TemplateHaskell #-}
module Client.DLightT
  ( module Client.DLightT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''DLightT

newDLightT :: DLightT
newDLightT =
  DLightT { _dlOrigin    = V3 0 0 0
          , _dlColor     = V3 0 0 0
          , _dlIntensity = 0
          }