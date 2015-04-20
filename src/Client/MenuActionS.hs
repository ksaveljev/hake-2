{-# LANGUAGE TemplateHaskell #-}
module Client.MenuActionS where

import Control.Lens (makeLenses)

import Client.MenuCommonS

data MenuActionS =
  MenuActionS { _maGeneric :: MenuCommonS
              }

makeLenses ''MenuActionS

newMenuActionS :: MenuActionS
newMenuActionS =
  MenuActionS { _maGeneric = newMenuCommonS
              }
