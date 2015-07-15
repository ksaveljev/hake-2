module Render.Fast.Light where

import Linear (V3)

import Quake
import QuakeState

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint _ = do
    io (putStrLn "Light.rLightPoint") >> undefined -- TODO
