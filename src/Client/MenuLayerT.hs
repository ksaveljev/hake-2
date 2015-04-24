{-# LANGUAGE TemplateHaskell #-}
module Client.MenuLayerT ( MenuLayerT
                         , module Client.MenuLayerT
                         ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MenuLayerT

newMenuLayerT :: MenuLayerT
newMenuLayerT =
  MenuLayerT { _mlDraw = Nothing
             , _mlKey  = Nothing
             }
