{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushT
  ( module QCommon.CBrushT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CBrushT

newCBrushT :: CBrushT
newCBrushT = CBrushT 0 0 0 0