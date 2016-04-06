module Render.Fast.Light
  ( rLightPoint
  , rPushDLights
  , rRenderDLights
  ) where

import Types

import Linear (V3)

rPushDLights :: Quake ()
rPushDLights = error "Light.rPushDLights" -- TODO

rRenderDLights :: Quake ()
rRenderDLights = error "Light.rRenderDLights" -- TODO

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint = error "Light.rLightPoint" -- TODO