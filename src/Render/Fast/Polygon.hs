module Render.Fast.Polygon
    ( create
    , reset
    , getPolyS1
    , getPolyS2
    , getPolyT1
    , getPolyT2
    , getPolyX
    , getPolyY
    , getPolyZ
    , setPolyS1
    , setPolyS2
    , setPolyT1
    , setPolyT2
    , setPolyX
    , setPolyY
    , setPolyZ
    ) where

import           Control.Lens                 (use, (^.), (.=), (+=), (&), (.~))
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector.Storable.Mutable as MSV

import           QuakeIOState
import           QuakeRef
import           QuakeState
import           Render.GLPolyT
import           Types

reset :: Quake ()
reset = do
    fastRenderAPIGlobals.frPolygonBufferIndex .= 0
    fastRenderAPIGlobals.frPolygonCount .= 0

create :: Int -> Quake (Ref GLPolyT)
create numVerts = do
    polyCount <- use (fastRenderAPIGlobals.frPolygonCount)
    bufferIndex <- use (fastRenderAPIGlobals.frPolygonBufferIndex)
    request $ do
        polygonCache <- use frPolygonCache
        io (MV.write polygonCache polyCount (newGLPolyT & glpNumVerts .~ numVerts
                                                        & glpPos .~ bufferIndex))
    fastRenderAPIGlobals.frPolygonCount += 1
    fastRenderAPIGlobals.frPolygonBufferIndex += numVerts
    return (Ref polyCount)

getPolygonBufferValue :: Int -> Ref GLPolyT -> Int -> Quake Float
getPolygonBufferValue offset polyRef idx = do
    poly <- readRef polyRef
    request $ do
        buffer <- use frPolygonBuffer
        io (MSV.read buffer ((idx + (poly^.glpPos)) * stride + offset))

setPolygonBufferValue :: Int -> Ref GLPolyT -> Int -> Float -> Quake ()
setPolygonBufferValue offset polyRef idx value = do
    poly <- readRef polyRef
    request $ do
        buffer <- use frPolygonBuffer
        io (MSV.write buffer ((idx + (poly^.glpPos)) * stride + offset) value)

getPolyX :: Ref GLPolyT -> Int -> Quake Float
getPolyX = getPolygonBufferValue 2

setPolyX :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyX = setPolygonBufferValue 2

getPolyY :: Ref GLPolyT -> Int -> Quake Float
getPolyY = getPolygonBufferValue 3

setPolyY :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyY = setPolygonBufferValue 3

getPolyZ :: Ref GLPolyT -> Int -> Quake Float
getPolyZ = getPolygonBufferValue 4

setPolyZ :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyZ = setPolygonBufferValue 4

getPolyS1 :: Ref GLPolyT -> Int -> Quake Float
getPolyS1 = getPolygonBufferValue 0

setPolyS1 :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyS1 = setPolygonBufferValue 0

getPolyT1 :: Ref GLPolyT -> Int -> Quake Float
getPolyT1 = getPolygonBufferValue 1

setPolyT1 :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyT1 = setPolygonBufferValue 1

getPolyS2 :: Ref GLPolyT -> Int -> Quake Float
getPolyS2 = getPolygonBufferValue 5

setPolyS2 :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyS2 = setPolygonBufferValue 5

getPolyT2 :: Ref GLPolyT -> Int -> Quake Float
getPolyT2 = getPolygonBufferValue 6

setPolyT2 :: Ref GLPolyT -> Int -> Float -> Quake ()
setPolyT2 = setPolygonBufferValue 6