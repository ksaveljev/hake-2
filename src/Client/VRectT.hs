{-# LANGUAGE TemplateHaskell #-}
module Client.VRectT where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''VRectT

newVRectT :: VRectT
newVRectT = VRectT
    { _vrX      = 0
    , _vrY      = 0
    , _vrWidth  = 0
    , _vrHeight = 0
    }
