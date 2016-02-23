{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushSideT
  ( module QCommon.CBrushSideT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CBrushSideT

newCBrushSideT :: CBrushSideT
newCBrushSideT = CBrushSideT Nothing Nothing