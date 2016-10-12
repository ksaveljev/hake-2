module Render.Fast.Warp
    ( glSubdivideSurface
    , rSetSky
    ) where

import           Control.Lens    (use)
import qualified Data.ByteString as B
import           Linear          (V3)

import           QuakeRef
import           QuakeState
import           Types

rSetSky :: B.ByteString -> Float -> V3 Float -> Quake ()
rSetSky = error "Warp.rSetSky" -- TODO

glSubdivideSurface :: Ref MSurfaceT -> Quake ()
glSubdivideSurface surfRef = do
    loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
    surf <- readRef surfRef
    model <- readRef loadModelRef
    error "Warp.glSubdivideSurface" -- TODO