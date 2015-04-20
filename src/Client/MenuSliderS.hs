{-# LANGUAGE TemplateHaskell #-}
module Client.MenuSliderS where

import Control.Lens (makeLenses)

import Client.MenuCommonS

data MenuSliderS =
  MenuSliderS { _msGeneric  :: MenuCommonS
              , _msMinValue :: Float
              , _msMaxValue :: Float
              , _msCurValue :: Float
              , _msRange    :: Float
              }

makeLenses ''MenuSliderS

newMenuSliderS :: MenuSliderS
newMenuSliderS =
  MenuSliderS { _msGeneric  = newMenuCommonS
              , _msMinValue = 0
              , _msMaxValue = 0
              , _msCurValue = 0
              , _msRange    = 0
              }
