module Util.Math3D
  ( angleToShort
  , angleVectors
  , calcFov
  , projectSource
  , rotatePointAroundVector
  , shortToAngle
  ) where

import           Control.Lens ((^.))
import           Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..), dot, normalize, cross, _x, _y, _z)

piRatio :: Float
piRatio = pi / 360

shortRatio :: Float
shortRatio = 360.0 / 65536.0

planeXYZ :: V.Vector (V3 Float)
planeXYZ = V.fromList [V3 1 0 0, V3 0 1 0, V3 0 0 1]

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
shortToAngle x = fromIntegral x * shortRatio

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

rotatePointAroundVector :: V3 Float -> V3 Float -> Float -> V3 Float
rotatePointAroundVector dir point degrees =
    let vf = dir
        vr = perpendicularVector dir
        vup = vr `cross` vf
        m = UV.fromList [ vr^._x, vup^._x, vf^._x
                        , vr^._y, vup^._y, vf^._y
                        , vr^._z, vup^._z, vf^._z
                        ]
        im = UV.fromList [ m UV.! 0, m UV.! 3, m UV.! 6
                         , m UV.! 1, m UV.! 4, m UV.! 7
                         , m UV.! 2, m UV.! 5, m UV.! 8
                         ]
        rDeg = degToRad degrees
        cosDeg = cos rDeg
        sinDeg = sin rDeg
        zrot = UV.fromList [  cosDeg, sinDeg, 0
                           , -sinDeg, cosDeg, 0
                           ,       0,      0, 1
                           ]
        tmpmat = concatRotations m zrot
        zrot' = concatRotations tmpmat im
        a = (zrot' UV.! 0) * (point^._x) + (zrot' UV.! 1) * (point^._y) + (zrot' UV.! 2) * (point^._z)
        b = (zrot' UV.! 3) * (point^._x) + (zrot' UV.! 4) * (point^._y) + (zrot' UV.! 5) * (point^._z)
        c = (zrot' UV.! 6) * (point^._x) + (zrot' UV.! 7) * (point^._y) + (zrot' UV.! 8) * (point^._z)
    in V3 a b c

perpendicularVector :: V3 Float -> V3 Float
perpendicularVector src =
  let xyz = findMinElem 0 1.0 (planeXYZ V.! 0)
      dst = projectPointOnPlane xyz src
  in normalize dst
  where findMinElem idx minElem xyz
          | idx >= 3 = xyz
          | otherwise =
              let v = case idx of
                        0 -> abs (src^._x)
                        1 -> abs (src^._y)
                        2 -> abs (src^._z)
                        _ -> undefined -- should never happen
              in if v < minElem
                   then findMinElem (idx + 1) v (planeXYZ V.! idx)
                   else findMinElem (idx + 1) minElem xyz

projectPointOnPlane :: V3 Float -> V3 Float -> V3 Float
projectPointOnPlane p normal =
  let invDenom = 1.0 / (normal `dot` normal)
      d = (normal `dot` p) * invDenom
      dst = fmap (* invDenom) normal
  in p - fmap (* d) dst

degToRad :: Float -> Float
degToRad degrees = degrees * pi / 180

concatRotations :: UV.Vector Float -> UV.Vector Float -> UV.Vector Float
concatRotations in1 in2 =
  UV.fromList [ (in1 UV.! 0) * (in2 UV.! 0) + (in1 UV.! 1) * (in2 UV.! 3) + (in1 UV.! 2) * (in2 UV.! 6)
              , (in1 UV.! 0) * (in2 UV.! 1) + (in1 UV.! 1) * (in2 UV.! 4) + (in1 UV.! 2) * (in2 UV.! 7)
              , (in1 UV.! 0) * (in2 UV.! 2) + (in1 UV.! 1) * (in2 UV.! 5) + (in1 UV.! 2) * (in2 UV.! 8)
              , (in1 UV.! 3) * (in2 UV.! 0) + (in1 UV.! 4) * (in2 UV.! 3) + (in1 UV.! 5) * (in2 UV.! 6)
              , (in1 UV.! 3) * (in2 UV.! 1) + (in1 UV.! 4) * (in2 UV.! 4) + (in1 UV.! 5) * (in2 UV.! 7)
              , (in1 UV.! 3) * (in2 UV.! 2) + (in1 UV.! 4) * (in2 UV.! 5) + (in1 UV.! 5) * (in2 UV.! 8)
              , (in1 UV.! 6) * (in2 UV.! 0) + (in1 UV.! 7) * (in2 UV.! 3) + (in1 UV.! 8) * (in2 UV.! 6)
              , (in1 UV.! 6) * (in2 UV.! 1) + (in1 UV.! 7) * (in2 UV.! 4) + (in1 UV.! 8) * (in2 UV.! 7)
              , (in1 UV.! 6) * (in2 UV.! 2) + (in1 UV.! 7) * (in2 UV.! 5) + (in1 UV.! 8) * (in2 UV.! 8)
              ]