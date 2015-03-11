{-# LANGUAGE TemplateHaskell #-}
module Client.DLightT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

data DLightT =
  DLightT { _dlOrigin    :: V3 Float
          , _dlColor     :: V3 Float
          , _dlIntensity :: V3 Float
          }

makeLenses ''DLightT
