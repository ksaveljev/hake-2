{-# LANGUAGE TemplateHaskell #-}
module Client.VRectT
  ( module Client.VRectT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''VRectT

newVRectT :: VRectT
newVRectT =
  VRectT { _vrX      = 0
         , _vrY      = 0
         , _vrWidth  = 0
         , _vrHeight = 0
         }