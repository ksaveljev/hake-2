{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushSideT where

import Control.Lens (makeLenses)

data CBrushSideT =
  CBrushSideT { cbsPlane   :: Maybe Int -- index of cmGlobals.cmMapPlanes
              , cbsSurface :: Maybe Int -- index of cmGlobals.cmMapSurfaces
              }

makeLenses ''CBrushSideT

newCBrushSideT :: CBrushSideT
newCBrushSideT = CBrushSideT Nothing Nothing
