module Render.Fast.Light where

import Linear (V3)

import Quake
import QuakeState

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint _ = do
    io (putStrLn "Light.rLightPoint") >> undefined -- TODO

rPushDLights :: Quake ()
rPushDLights = do
    io (putStrLn "Light.rPushDLights") >> undefined -- TODO

rRenderDLights :: Quake ()
rRenderDLights = do
    io (putStrLn "Light.rRenderDLights") >> undefined -- TODO
