{-# LANGUAGE TemplateHaskell #-}
module QCommon.CNodeT where

import Control.Lens (makeLenses)

data CNodeT =
  CNodeT { _cnPlane    :: Maybe Int -- index of cmGlobals.cmMapPlanes
         , _cnChildren :: (Int, Int)
         }

makeLenses ''CNodeT

newCNodeT :: CNodeT
newCNodeT = CNodeT Nothing (0, 0)
