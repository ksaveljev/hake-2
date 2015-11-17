{-# LANGUAGE TemplateHaskell #-}
module Client.MenuLayerT ( MenuLayerT
                         , module Client.MenuLayerT
                         , module Client.KeyFuncT
                         ) where

import Control.Lens (makeLenses)

import Internal
import Client.KeyFuncT

makeLenses ''MenuLayerT

newMenuLayerT :: MenuLayerT
newMenuLayerT =
  MenuLayerT { _mlDraw = Nothing
             , _mlKey  = Nothing
             }
