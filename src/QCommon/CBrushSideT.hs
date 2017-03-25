{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushSideT
    ( module QCommon.CBrushSideT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CBrushSideT

newCBrushSideT :: CBrushSideT
newCBrushSideT = CBrushSideT Nothing Nothing