{-# LANGUAGE TemplateHaskell #-}
module Client.MenuLayerT where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MenuLayerT

newMenuLayerT :: MenuLayerT
newMenuLayerT = MenuLayerT
    { _mlDraw = Nothing
    , _mlKey  = Nothing
    }
