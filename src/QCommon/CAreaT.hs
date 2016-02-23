{-# LANGUAGE TemplateHaskell #-}
module QCommon.CAreaT
  ( module QCommon.CAreaT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CAreaT

newCAreaT :: CAreaT
newCAreaT = CAreaT 0 0 0 0