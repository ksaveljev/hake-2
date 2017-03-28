{-# LANGUAGE TemplateHaskell #-}
module Client.LightStyleT where

import Linear (V3(..))
import Control.Lens (makeLenses)

data LightStyleT =
  LightStyleT { _lsRGB   :: V3 Float
              , _lsWhite :: Float
              }

makeLenses ''LightStyleT

newLightStyleT :: LightStyleT
newLightStyleT =
  LightStyleT { _lsRGB   = V3 0 0 0
              , _lsWhite = 0
              }
