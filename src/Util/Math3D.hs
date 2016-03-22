module Util.Math3D
  ( angleToShort
  , angleVectors
  , calcFov
  , projectSource
  , shortToAngle
  ) where

import Control.Lens ((^.))
import Data.Int (Int16)
import Linear (V3(..), _x, _y, _z)

piRatio :: Float
piRatio = pi / 360

shortRatio :: Float
shortRatio = 360.0 / 65536.0

angleVectors :: V3 Float -> Bool -> Bool -> Bool -> (V3 Float, V3 Float, V3 Float)
angleVectors angles setForward setRight setUp = (forward, right, up)
  where cr = 2 * piRatio
        angle = (angles^._y) * cr
        sy = sin angle
        cy = cos angle
        angle' = (angles^._x) * cr
        sp = sin angle'
        cp = cos angle'
        emptyV3 = V3 0 0 0
        forward | setForward = V3 (cp * cy) (cp * sy) (-sp)
                | otherwise = emptyV3
        angle'' = (angles^._z) * cr
        sr = sin angle''
        cr' = cos angle''
        right | setRight = V3 ((-sr) * sp * cy + cr' * sy) ((-sr) * sp * sy + (-cr') * cy) ((-sr) * cp)
              | otherwise = emptyV3
        up | setUp = V3 (cr' * sp * cy + sr * sy) (cr' * sp * sy + (-sr) * cy) (cr' * cp)
           | otherwise = emptyV3

projectSource :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
projectSource point distance forward right = V3 a b c
  where a = (point^._x) + (forward^._x) * (distance^._x) + (right^._x) * (distance^._y)
        b = (point^._y) + (forward^._y) * (distance^._x) + (right^._y) * (distance^._y)
        c = (point^._z) + (forward^._z) * (distance^._x) + (right^._z) * (distance^._y) + (distance^._z)

shortToAngle :: Int -> Float
shortToAngle x = (fromIntegral x) * shortRatio

angleToShort :: Float -> Int16
angleToShort x = truncate (x / shortRatio)

calcFov :: Float -> Float -> Float -> Float
calcFov fovX width height =
    -- TODO: do we need this?? when can this happen?
    {-
    when (fovX < 1.0 || fovX > 179.0) $
      Com.comError Constants.errDrop ("Bad fov: " `B.append` BC.pack (show fovX))
    -}
    let x = width / tan (fovX * piRatio)
        a = atan (height / x)
    in a / piRatio