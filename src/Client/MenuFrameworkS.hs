{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.MenuFrameworkS ( MenuFrameworkS(..)
                             , module Client.MenuFrameworkS
                             , module Client.MenuItem
                             ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.MenuItem

makeLenses ''MenuFrameworkS

newMenuFrameworkS :: MenuFrameworkS
newMenuFrameworkS =
  MenuFrameworkS { _mfX          = 0
                 , _mfY          = 0
                 , _mfCursor     = 0
                 , _mfNItems     = 0
                 , _mfNSlots     = 0
                 , _mfItems      = V.empty
                 , _mfStatusBar  = Nothing
                 , _mfCursorDraw = Nothing
                 }
