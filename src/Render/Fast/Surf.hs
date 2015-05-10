module Render.Fast.Surf where

import Quake
import QuakeState

glBeginBuildingLightmaps :: ModelReference -> Quake ()
glBeginBuildingLightmaps (ModKnownReference modelIdx) = do
    io (putStrLn "Surf.glBeginBuildingLightmaps") >> undefined -- TODO
