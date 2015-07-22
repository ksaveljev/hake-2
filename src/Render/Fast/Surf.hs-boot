module Render.Fast.Surf where

import Data.IORef (IORef)

import Quake
import QuakeState

glBeginBuildingLightmaps :: IORef ModelT -> Quake ()

glEndBuildingLightmaps :: Quake ()

glCreateSurfaceLightmap :: MSurfaceT -> Quake MSurfaceT

glBuildPolygonFromSurface :: MSurfaceT -> Quake MSurfaceT

rMarkLeaves :: Quake ()

rDrawWorld :: Quake ()

rDrawAlphaSurfaces :: Quake ()
