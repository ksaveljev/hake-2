{-# LANGUAGE TemplateHaskell #-}
module QCommon.CLeafT
  ( module QCommon.CLeafT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CLeafT

newCLeafT :: CLeafT
newCLeafT = CLeafT 0 0 0 0 0