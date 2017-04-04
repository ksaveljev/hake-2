{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushT where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CBrushT

newCBrushT :: CBrushT
newCBrushT = CBrushT 0 0 0 0