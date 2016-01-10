{-# LANGUAGE TemplateHaskell #-}
module Client.MenuItem ( MenuListS(..)
                       , MenuActionS(..)
                       , MenuSliderS(..)
                       , MenuSeparatorS(..)
                       , MenuFieldS(..)
                       , module Client.MenuItem
                       , module Client.MenuCommonS
                       ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.MenuCommonS

makeLenses ''MenuListS
makeLenses ''MenuSliderS
makeLenses ''MenuActionS
makeLenses ''MenuSeparatorS

newMenuListS :: MenuListS
newMenuListS =
  MenuListS { _mlGeneric   = newMenuCommonS
            , _mlCurValue  = 0
            , _mlItemNames = V.empty
            }

newMenuSliderS :: MenuSliderS
newMenuSliderS =
  MenuSliderS { _msGeneric  = newMenuCommonS
              , _msMinValue = 0
              , _msMaxValue = 0
              , _msCurValue = 0
              , _msRange    = 0
              }

newMenuActionS :: MenuActionS
newMenuActionS =
  MenuActionS { _maGeneric = newMenuCommonS
              }

newMenuSeparatorS :: MenuSeparatorS
newMenuSeparatorS =
  MenuSeparatorS { _mspGeneric = newMenuCommonS
                 }

newMenuFieldS :: MenuFieldS
newMenuFieldS =
  MenuFieldS { _mflGeneric       = newMenuCommonS
             , _mflBuffer        = Nothing
             , _mflCursor        = 0
             , _mflLength        = 0
             , _mflVisibleLength = 0
             , _mflVisibleOffset = 0
             }
