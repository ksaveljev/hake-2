{-# LANGUAGE TemplateHaskell #-}
module Client.MenuSeparatorS
    ( module Client.MenuSeparatorS
    ) where

import           Control.Lens (makeLenses)

import           Client.MenuCommonS
import           Types

makeLenses ''MenuSeparatorS

newMenuSeparatorS :: MenuSeparatorS
newMenuSeparatorS = MenuSeparatorS
    { _mspGeneric = newMenuCommonS
    }