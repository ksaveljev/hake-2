module Render.Fast.Surf where

import Data.IORef (IORef)

import Quake
import QuakeState

glBeginBuildingLightmaps :: IORef ModelT -> Quake ()

glEndBuildingLightmaps :: Quake ()

glCreateSurfaceLightmap :: IORef MSurfaceT -> Quake ()

glBuildPolygonFromSurface :: IORef MSurfaceT -> Quake ()

rMarkLeaves :: Quake ()

rDrawWorld :: Quake ()

rDrawAlphaSurfaces :: Quake ()
