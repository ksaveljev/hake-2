{-# LANGUAGE TemplateHaskell #-}
module Client.VRectT where

import Control.Lens (makeLenses)

data VRectT =
  VRectT { _vrX      :: Int
         , _vrY      :: Int
         , _vrWidth  :: Int
         , _vrHeight :: Int
         }

makeLenses ''VRectT
