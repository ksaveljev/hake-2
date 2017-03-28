{-# LANGUAGE TemplateHaskell #-}
module Client.DLightT where

import Linear (V3(..))
import Control.Lens (makeLenses)

data DLightT =
  DLightT { _dlOrigin    :: V3 Float
          , _dlColor     :: V3 Float
          , _dlIntensity :: Float
          }

makeLenses ''DLightT

newDLightT :: DLightT
newDLightT =
  DLightT { _dlOrigin    = V3 0 0 0
          , _dlColor     = V3 0 0 0
          , _dlIntensity = 0
          }
