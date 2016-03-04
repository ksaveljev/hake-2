{-# LANGUAGE TemplateHaskell #-}
module Client.LightStyleT
  ( module Client.LightStyleT
  ) where

import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''LightStyleT

newLightStyleT :: LightStyleT
newLightStyleT =
  LightStyleT { _lsRGB   = V3 0 0 0
              , _lsWhite = 0
              }