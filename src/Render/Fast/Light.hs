module Render.Fast.Light
  ( rBuildLightMap
  , rLightPoint
  , rPushDLights
  , rRenderDLights
  , rSetCacheState
  ) where

import           Types

import qualified Data.Vector.Storable as SV
import           Data.Word (Word8)
import           Linear (V3)

rPushDLights :: Quake ()
rPushDLights = error "Light.rPushDLights" -- TODO

rRenderDLights :: Quake ()
rRenderDLights = error "Light.rRenderDLights" -- TODO

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint = error "Light.rLightPoint" -- TODO

rSetCacheState :: Ref MSurfaceT -> Quake ()
rSetCacheState = error "Light.rSetCacheState" -- TODO

rBuildLightMap :: Ref MSurfaceT -> SV.Vector Word8 -> Int -> Int -> Quake ()
rBuildLightMap = error "Light.rBuildLightMap" -- TODO