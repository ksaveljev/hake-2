module Render.Fast.Surf where

import Quake
import QuakeState

glBeginBuildingLightmaps :: ModelReference -> Quake ()

glEndBuildingLightmaps :: Quake ()

glCreateSurfaceLightmap :: MSurfaceT -> Quake MSurfaceT

glBuildPolygonFromSurface :: MSurfaceT -> Quake MSurfaceT

lmUploadBlock :: Bool -> Quake ()

rMarkLeaves :: Quake ()

rDrawWorld :: Quake ()

rDrawAlphaSurfaces :: Quake ()
