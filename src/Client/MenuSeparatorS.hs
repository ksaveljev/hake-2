{-# LANGUAGE TemplateHaskell #-}
module Client.MenuSeparatorS where

import Control.Lens (makeLenses)

import Client.MenuCommonS

data MenuSeparatorS =
  MenuSeparatorS { _maGeneric :: MenuCommonS
                 }

makeLenses ''MenuSeparatorS

newMenuSeparatorS :: MenuSeparatorS
newMenuSeparatorS =
  MenuSeparatorS { _maGeneric = newMenuCommonS
                 }
