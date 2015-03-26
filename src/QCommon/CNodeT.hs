{-# LANGUAGE TemplateHaskell #-}
module QCommon.CNodeT where

import Control.Lens (makeLenses)

import Game.CPlaneT

data CNodeT =
  CNodeT { _cnPlane    :: Maybe CPlaneT
         , _cnChildren :: (Int, Int)
         }

makeLenses ''CNodeT

newCNodeT :: CNodeT
newCNodeT = CNodeT Nothing (0, 0)
