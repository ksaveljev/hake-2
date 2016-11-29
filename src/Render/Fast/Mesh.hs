module Render.Fast.Mesh
    ( myGLUPerspective
    ) where

import           Control.Lens    (use, (^.))
import           GHC.Float       (float2Double)
import qualified Graphics.GL     as GL

import           QuakeState
import           Render.GLStateT
import           Types

myGLUPerspective :: Double -> Double -> Double -> Double -> Quake ()
myGLUPerspective fovY aspect zNear zFar = do
    glState <- use (fastRenderAPIGlobals.frGLState)
    request (setupPerspective glState fovY aspect zNear zFar)

setupPerspective :: GLStateT -> Double -> Double -> Double -> Double -> QuakeIO ()
setupPerspective glState fovY aspect zNear zFar =
    GL.glFrustum (realToFrac xmin')
                 (realToFrac xmax')
                 (realToFrac ymin)
                 (realToFrac ymax)
                 (realToFrac zNear)
                 (realToFrac zFar)
  where
    ymax = zNear * tan (fovY * pi / 360)
    ymin = negate ymax
    xmin = ymin * aspect
    xmax = ymax * aspect
    xmin' = xmin - (2 * float2Double (glState^.glsCameraSeparation)) / zNear
    xmax' = xmax - (2 * float2Double (glState^.glsCameraSeparation)) / zNear