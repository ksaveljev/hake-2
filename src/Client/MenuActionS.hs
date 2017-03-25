{-# LANGUAGE TemplateHaskell #-}
module Client.MenuActionS
    ( module Client.MenuActionS
    ) where

import           Control.Lens       (makeLenses)

import           Client.MenuCommonS
import           Types
makeLenses ''MenuActionS

newMenuActionS :: MenuActionS
newMenuActionS = MenuActionS
    { _maGeneric = newMenuCommonS
    }