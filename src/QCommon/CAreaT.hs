{-# LANGUAGE TemplateHaskell #-}
module QCommon.CAreaT where

import Control.Lens (makeLenses)

data CAreaT =
  CAreaT { _caNumAreaPortals  :: Int
         , _caFirstAreaPortal :: Int
         , _caFloodNum        :: Int
         , _caFloodValid      :: Int
         }

makeLenses ''CAreaT

newCAreaT :: CAreaT
newCAreaT = CAreaT 0 0 0 0
