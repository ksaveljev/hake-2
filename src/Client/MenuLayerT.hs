{-# LANGUAGE TemplateHaskell #-}
module Client.MenuLayerT
  ( module Client.MenuLayerT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''MenuLayerT

newMenuLayerT :: MenuLayerT
newMenuLayerT =
  MenuLayerT { _mlDraw = Nothing
             , _mlKey  = Nothing
             }