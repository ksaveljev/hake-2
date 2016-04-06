module Render.Fast.Polygon
  ( reset
  ) where

import QuakeState
import Types

import Control.Lens ((.=))

reset :: Quake ()
reset =
  do fastRenderAPIGlobals.frPolygonBufferIndex .= 0
     fastRenderAPIGlobals.frPolygonCount .= 0
