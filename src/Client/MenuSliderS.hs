{-# LANGUAGE TemplateHaskell #-}
module Client.MenuSliderS
  ( module Client.MenuSliderS
  ) where

import Client.MenuCommonS
import Types

import Control.Lens (makeLenses)

makeLenses ''MenuSliderS

newMenuSliderS :: MenuSliderS
newMenuSliderS =
  MenuSliderS { _msGeneric  = newMenuCommonS
              , _msMinValue = 0
              , _msMaxValue = 0
              , _msCurValue = 0
              , _msRange    = 0
              }