{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushSideT where

import Control.Lens (makeLenses)

import Game.CPlaneT
import Game.MapSurfaceT

data CBrushSideT =
  CBrushSideT { _cbsPlane   :: Maybe CPlaneT
              , _cbsSurface :: Maybe MapSurfaceT
              }

makeLenses ''CBrushSideT

newCBrushSideT :: CBrushSideT
newCBrushSideT = CBrushSideT Nothing Nothing
