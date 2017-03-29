{-# LANGUAGE TemplateHaskell #-}
module Client.LightStyleT where

import           Control.Lens (makeLenses)
import           Linear       (V3(..))

import           Types

makeLenses ''LightStyleT

newLightStyleT :: LightStyleT
newLightStyleT = LightStyleT
    { _lsRGB   = V3 0 0 0
    , _lsWhite = 0
    }
