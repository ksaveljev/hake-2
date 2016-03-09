{-# LANGUAGE TemplateHaskell #-}
module Client.MenuActionS
  ( module Client.MenuActionS
  ) where

import Client.MenuCommonS
import Types

import Control.Lens (makeLenses)

makeLenses ''MenuActionS

newMenuActionS :: MenuActionS
newMenuActionS =
  MenuActionS { _maGeneric = newMenuCommonS
              }