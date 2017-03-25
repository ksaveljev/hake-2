{-# LANGUAGE TemplateHaskell #-}
module Client.MenuFrameworkS
    ( module Client.MenuFrameworkS
    ) where

import           Control.Lens (makeLenses)
import qualified Data.Vector  as V

import           Types

makeLenses ''MenuFrameworkS

newMenuFrameworkS :: MenuFrameworkS
newMenuFrameworkS = MenuFrameworkS
    { _mfX          = 0
    , _mfY          = 0
    , _mfCursor     = 0
    , _mfNItems     = 0
    , _mfNSlots     = 0
    , _mfItems      = V.empty
    , _mfStatusBar  = Nothing
    , _mfCursorDraw = Nothing
    }