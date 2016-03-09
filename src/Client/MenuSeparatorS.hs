{-# LANGUAGE TemplateHaskell #-}
module Client.MenuSeparatorS
  ( module Client.MenuSeparatorS
  ) where

import Client.MenuCommonS
import Types

import Control.Lens (makeLenses)

makeLenses ''MenuSeparatorS

newMenuSeparatorS :: MenuSeparatorS
newMenuSeparatorS =
  MenuSeparatorS { _mspGeneric = newMenuCommonS
                 }