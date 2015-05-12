module Render.Fast.Polygon where

import Control.Lens (use, (.=), (+=), ix)

import Quake
import QuakeState

reset :: Quake ()
reset = do
    fastRenderAPIGlobals.frPolygonBufferIndex .= 0
    fastRenderAPIGlobals.frPolygonCount .= 0

create :: Int -> Quake GLPolyReference
create numVerts = do
    polyCount <- use $ fastRenderAPIGlobals.frPolygonCount
    bufferIndex <- use $ fastRenderAPIGlobals.frPolygonBufferIndex
    fastRenderAPIGlobals.frPolygonCache.ix polyCount .= newGLPolyT { _glpNumVerts = numVerts, _glpPos = bufferIndex }
    fastRenderAPIGlobals.frPolygonCount += 1
    fastRenderAPIGlobals.frPolygonBufferIndex += numVerts
    return (GLPolyReference polyCount)
