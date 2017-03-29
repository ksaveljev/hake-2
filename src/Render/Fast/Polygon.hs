module Render.Fast.Polygon where

import Control.Lens (use, (.=), (+=), ix, (^.), preuse)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MSV

import Types
import QuakeState

getPolygonBufferValue :: Int -> GLPolyReference -> Int -> Quake Float
getPolygonBufferValue offset (GLPolyReference polyIdx) idx = do
    polygonCache <- use $ fastRenderAPIGlobals.frPolygonCache
    poly <- io $ MV.read polygonCache polyIdx
    use (fastRenderAPIGlobals.frPolygonBuffer) >>= \buffer -> 
      io $ MSV.read buffer ((idx + (poly^.glpPos)) * stride + offset)

setPolygonBufferValue :: Int -> GLPolyReference -> Int -> Float -> Quake ()
setPolygonBufferValue offset (GLPolyReference polyIdx) idx value = do
    polygonCache <- use $ fastRenderAPIGlobals.frPolygonCache
    poly <- io $ MV.read polygonCache polyIdx
    use (fastRenderAPIGlobals.frPolygonBuffer) >>= \buffer ->
      io $ MSV.write buffer ((idx + (poly^.glpPos)) * stride + offset) value

getPolyX :: GLPolyReference -> Int -> Quake Float
getPolyX = getPolygonBufferValue 2

setPolyX :: GLPolyReference -> Int -> Float -> Quake ()
setPolyX = setPolygonBufferValue 2

getPolyY :: GLPolyReference -> Int -> Quake Float
getPolyY = getPolygonBufferValue 3

setPolyY :: GLPolyReference -> Int -> Float -> Quake ()
setPolyY = setPolygonBufferValue 3

getPolyZ :: GLPolyReference -> Int -> Quake Float
getPolyZ = getPolygonBufferValue 4

setPolyZ :: GLPolyReference -> Int -> Float -> Quake ()
setPolyZ = setPolygonBufferValue 4

getPolyS1 :: GLPolyReference -> Int -> Quake Float
getPolyS1 = getPolygonBufferValue 0

setPolyS1 :: GLPolyReference -> Int -> Float -> Quake ()
setPolyS1 = setPolygonBufferValue 0

getPolyT1 :: GLPolyReference -> Int -> Quake Float
getPolyT1 = getPolygonBufferValue 1

setPolyT1 :: GLPolyReference -> Int -> Float -> Quake ()
setPolyT1 = setPolygonBufferValue 1

getPolyS2 :: GLPolyReference -> Int -> Quake Float
getPolyS2 = getPolygonBufferValue 5

setPolyS2 :: GLPolyReference -> Int -> Float -> Quake ()
setPolyS2 = setPolygonBufferValue 5

getPolyT2 :: GLPolyReference -> Int -> Quake Float
getPolyT2 = getPolygonBufferValue 6

setPolyT2 :: GLPolyReference -> Int -> Float -> Quake ()
setPolyT2 = setPolygonBufferValue 6

beginScrolling :: GLPolyT -> Float -> Quake ()
beginScrolling _ _ = do
    io (putStrLn "GLPolyT.beginScrolling") >> undefined -- TODO

endScrolling :: GLPolyT -> Quake ()
endScrolling _ = do
    io (putStrLn "GLPolyT.endScrolling") >> undefined -- TODO

reset :: Quake ()
reset = do
    fastRenderAPIGlobals.frPolygonBufferIndex .= 0
    fastRenderAPIGlobals.frPolygonCount .= 0

create :: Int -> Quake GLPolyReference
create numVerts = do
    polyCount <- use $ fastRenderAPIGlobals.frPolygonCount
    bufferIndex <- use $ fastRenderAPIGlobals.frPolygonBufferIndex

    use (fastRenderAPIGlobals.frPolygonCache) >>= \polygonCache ->
      io $ MV.write polygonCache polyCount newGLPolyT { _glpNumVerts = numVerts, _glpPos = bufferIndex }

    fastRenderAPIGlobals.frPolygonCount += 1
    fastRenderAPIGlobals.frPolygonBufferIndex += numVerts
    return (GLPolyReference polyCount)
