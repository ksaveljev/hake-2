{-# LANGUAGE TemplateHaskell #-}
module QCommon.CAreaT where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CAreaT

newCAreaT :: CAreaT
newCAreaT = CAreaT 0 0 0 0
