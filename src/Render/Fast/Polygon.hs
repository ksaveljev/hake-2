module Render.Fast.Polygon where

import Control.Lens ((.=))

import Quake
import QuakeState

reset :: Quake ()
reset = do
    fastRenderAPIGlobals.frPolygonBufferIndex .= 0
    fastRenderAPIGlobals.frPolygonCount .= 0
