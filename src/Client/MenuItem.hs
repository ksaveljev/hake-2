{-# LANGUAGE TemplateHaskell #-}
module Client.MenuItem ( MenuItem(..)
                       , module Client.MenuItem
                       , module Client.MenuCommonS
                       ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.MenuCommonS

makeLenses ''MenuItem

newMenuListS :: MenuItem
newMenuListS =
  MenuListS { _mlGeneric   = newMenuCommonS
            , _mlCurValue  = 0
            , _mlItemNames = V.empty
            }

newMenuSliderS :: MenuItem
newMenuSliderS =
  MenuSliderS { _msGeneric  = newMenuCommonS
              , _msMinValue = 0
              , _msMaxValue = 0
              , _msCurValue = 0
              , _msRange    = 0
              }

newMenuActionS :: MenuItem
newMenuActionS =
  MenuActionS { _maGeneric = newMenuCommonS
              }
