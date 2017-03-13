module Util.Math3D
  ( angleMod
  , angleToShort
  , angleVectors
  , boxOnPlaneSide
  , calcFov
  , lerpAngles
  , projectSource
  , rotatePointAroundVector
  , shortToAngle
  , vectorAngles
  , vectorYaw
  , v3Access
  ) where

import           Control.Lens (Const, (^.))
import           Data.Bits ((.&.), (.|.))
import           Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..), dot, normalize, cross, _x, _y, _z)

import qualified Constants
import           Game.CPlaneT
import           Types

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

v3Access :: Int -> (a -> Const b a) -> V3 a -> Const b (V3 a)
v3Access v = case v of
                 0 -> _x
                 1 -> _y
                 2 -> _z
                 _ -> error "Math3D.v3Access shouldn't happen"

lerpAngles :: V3 Float -> V3 Float -> Float -> V3 Float
lerpAngles (V3 a b c) (V3 a' b' c') frac =
    V3 (lerpAngle a a' frac) (lerpAngle b b' frac) (lerpAngle c c' frac)

lerpAngle :: Float -> Float -> Float -> Float
lerpAngle a2 a1 frac =
    let a1' = if a1 - a2 > 180 then a1 - 360 else a1
        a1'' = if a1' - a2 < -180 then a1' + 360 else a1'
    in a2 + frac * (a1'' - a2)

angleMod :: Float -> Float
angleMod a = let b = truncate (a / shortRatio) :: Int
             in shortRatio * fromIntegral (b .&. 65535)

boxOnPlaneSide :: V3 Float -> V3 Float -> CPlaneT -> Int
boxOnPlaneSide emins emaxs p
    | (p^.cpType) < 3 = fastAxialCase
    | otherwise = -- general case
        let V3 a b c = p^.cpNormal
            V3 mina minb minc = emins
            V3 maxa maxb maxc = emaxs
            (dist1, dist2) =
                case p^.cpSignBits of
                    0 -> let d1 = a * maxa + b * maxb + c * maxc
                             d2 = a * mina + b * minb + c * minc
                         in (d1, d2)
                    1 -> let d1 = a * mina + b * maxb + c * maxc
                             d2 = a * maxa + b * minb + c * minc
                         in (d1, d2)
                    2 -> let d1 = a * maxa + b * minb + c * maxc
                             d2 = a * mina + b * maxb + c * minc
                         in (d1, d2)
                    3 -> let d1 = a * mina + b * minb + c * maxc
                             d2 = a * maxa + b * maxb + c * minc
                         in (d1, d2)
                    4 -> let d1 = a * maxa + b * maxb + c * minc
                             d2 = a * mina + b * minb + c * maxc
                         in (d1, d2)
                    5 -> let d1 = a * mina + b * maxb + c * minc
                             d2 = a * maxa + b * minb + c * maxc
                         in (d1, d2)
                    6 -> let d1 = a * maxa + b * minb + c * minc
                             d2 = a * mina + b * maxb + c * maxc
                         in (d1, d2)
                    7 -> let d1 = a * mina + b * minb + c * minc
                             d2 = a * maxa + b * maxb + c * maxc
                         in (d1, d2)
                    _ -> undefined -- TODO: throw error
            sides = if dist1 >= (p^.cpDist) then 1 else 0
            sides' = if dist2 < (p^.cpDist) then sides .|. 2 else sides
        -- TODO: 
        -- assert(sides != 0) : "BoxOnPlaneSide(): sides == 0 bug";
        in sides'
  where
    ptype = v3Access (fromIntegral (p^.cpType))
    fastAxialCase
        | (p^.cpDist) <= (emins^.ptype) = 1
        | (p^.cpDist) >= (emaxs^.ptype) = 2
        | otherwise                     = 3

vectorAngles :: V3 Float -> V3 Float
vectorAngles value1
    | (value1^._y) == 0 && (value1^._x) == 0 =
        let pitch = if (value1^._z) > 0 then 90 else 270
        in V3 (negate pitch) 0 0
    | otherwise =
        let yaw | (value1^._x) /= 0 = (atan2 (value1^._y) (value1^._x)) * 180 / pi
                | (value1^._y) > 0  = 90
                | otherwise         = (-90)
            yaw' = if yaw < 0 then yaw + 360 else yaw
            forward = sqrt ((value1^._x) * (value1^._x) + (value1^._y) * (value1^._y))
            pitch = truncate ((atan2 (value1^._z) forward) * 180 / pi) :: Integer
            pitch' = fromIntegral (if pitch < 0 then pitch + 360 else pitch)
        in V3 (negate pitch') yaw' 0

vectorYaw :: V3 Float -> Float
vectorYaw vec
    | vec^.pitchAccess == 0 =
        let yaw | vec^.yawAccess > 0 = 90
                | vec^.yawAccess < 0 = -90
                | otherwise          = 0
        in yaw
    | otherwise =
        let yaw = truncate ((atan2 (vec^.yawAccess) (vec^.pitchAccess)) * 180 / pi) :: Int
        in fromIntegral (if yaw < 0 then yaw + 360 else yaw)
  where
    pitchAccess = v3Access Constants.pitch
    yawAccess = v3Access Constants.yaw
