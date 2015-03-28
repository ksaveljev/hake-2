{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushSideT where

import Control.Lens (makeLenses)

data CBrushSideT =
  CBrushSideT { cbsPlane   :: Maybe Int -- index of cmGlobals.cmMapPlanes
              , cbsSurface :: Maybe Int -- index of cmGlobals.cmMapSurfaces, Nothing means nullsurface (from jake2)
              }

makeLenses ''CBrushSideT

newCBrushSideT :: CBrushSideT
newCBrushSideT = CBrushSideT Nothing Nothing
