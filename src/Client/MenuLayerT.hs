{-# LANGUAGE TemplateHaskell #-}
module Client.MenuLayerT where

import Control.Lens (makeLenses)

import Client.KeyFuncT
import QCommon.XCommandT

data MenuLayerT =
  MenuLayerT { _mlDraw :: Maybe XCommandT
             , _mlKey  :: Maybe KeyFuncT
             }

makeLenses ''MenuLayerT

newMenuLayerT :: MenuLayerT
newMenuLayerT =
  MenuLayerT { _mlDraw = Nothing
             , _mlKey  = Nothing
             }
