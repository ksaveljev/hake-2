{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Math3D where

import Control.Lens (preuse, ix, (^.), Const)
import Data.Bits ((.&.))
import Data.Int (Int16)
import Linear (V3(..), _x, _y, _z)

import Quake
import QuakeState
import qualified Constants

import qualified Debug.Trace as DT

shortRatio :: Float
shortRatio = 360 / 65536

piRatio :: Float
piRatio = pi / 360

v3Access :: Int -> (a -> Const b a) -> V3 a -> Const b (V3 a)
v3Access v = case v of
               0 -> _x
               1 -> _y
               2 -> _z
               _ -> DT.trace "OHNO" $ undefined -- shouldn't happen

                                          -- index of cmGlobals.cmMapPlanes
boxOnPlaneSide :: V3 Float -> V3 Float -> Int -> Quake Int
boxOnPlaneSide emins emaxs pIdx = do
    Just p <- preuse $ cmGlobals.cmMapPlanes.ix pIdx

    let ptype = v3Access (fromIntegral $ p^.cpType)

    if (p^.cpType) < 3
      then -- fast axial cases
        if | (p^.cpDist) <= (emins^.ptype) -> return 1
           | (p^.cpDist) >= (emaxs^.ptype) -> return 2
           | otherwise -> return 3
      else do -- general case
        let V3 a b c = p^.cpNormal
            V3 mina minb minc = emins
            V3 maxa maxb maxc = emaxs
        (dist1, dist2) <- case p^.cpSignBits of
                            0 -> let d1 = a * maxa + b * maxb + c * maxc
                                     d2 = a * mina + b * minb + c * minc
                                 in return (d1, d2)
                            1 -> let d1 = a * mina + b * maxb + c * maxc
                                     d2 = a * maxa + b * minb + c * minc
                                 in return (d1, d2)
                            2 -> let d1 = a * maxa + b * minb + c * maxc
                                     d2 = a * mina + b * maxb + c * minc
                                 in return (d1, d2)
                            3 -> let d1 = a * mina + b * minb + c * maxc
                                     d2 = a * maxa + b * maxb + c * minc
                                 in return (d1, d2)
                            4 -> let d1 = a * maxa + b * maxb + c * minc
                                     d2 = a * mina + b * minb + c * maxc
                                 in return (d1, d2)
                            5 -> let d1 = a * mina + b * maxb + c * minc
                                     d2 = a * maxa + b * minb + c * maxc
                                 in return (d1, d2)
                            6 -> let d1 = a * maxa + b * minb + c * minc
                                     d2 = a * mina + b * maxb + c * maxc
                                 in return (d1, d2)
                            7 -> let d1 = a * mina + b * minb + c * minc
                                     d2 = a * maxa + b * maxb + c * maxc
                                 in return (d1, d2)
                            _ -> io (putStrLn "Math3D.boxOnPlaneSide") >> undefined -- TODO: throw error

        let sides = if | dist1 >= (p^.cpDist) -> 1
                       | dist2 < (p^.cpDist) -> 2
                       | otherwise -> 0

        -- TODO: 
        -- assert(sides != 0) : "BoxOnPlaneSide(): sides == 0 bug";

        return sides

angleVectors :: V3 Float -> Bool -> Bool -> Bool -> (Maybe (V3 Float), Maybe (V3 Float), Maybe (V3 Float))
angleVectors angles setForward setRight setUp =
    let cr = 2 * piRatio
        angle = (angles^.(v3Access Constants.yaw)) * cr
        sy = sin angle
        cy = cos angle
        angle' = (angles^.(v3Access Constants.pitch)) * cr
        sp = sin angle'
        cp = cos angle'
        forward = if setForward
                    then Just $ V3 (cp * cy) (cp * sy) (-sp)
                    else Nothing
        angle'' = (angles^.(v3Access Constants.roll)) * cr
        sr = sin angle''
        cr' = cos angle''
        right = if setRight
                  then Just $ V3 ((-sr) * sp * cy + cr' * sy) ((-sr) * sp * sy + (-cr') * cy) ((-sr) * cp)
                  else Nothing
        up = if setUp
               then Just $ V3 (cr' * sp * cy + sr * sy) (cr' * sp * sy + (-sr) * cy) (cr' * cp)
               else Nothing
    in (forward, right, up)

shortToAngle :: Int -> Float
shortToAngle x = (fromIntegral x) * shortRatio

angleToShort :: Float -> Int16
angleToShort x = truncate (x / shortRatio)

-- TODO: use Double here???
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

vectorYaw :: V3 Float -> Float
vectorYaw vec =
    let pitchAccess = v3Access Constants.pitch
        yawAccess = v3Access Constants.yaw
    in if vec^.pitchAccess == 0
         then if | vec^.yawAccess > 0 -> 90
                 | vec^.yawAccess < 0 -> -90
                 | otherwise -> 0
         else let yaw :: Int = truncate $ (atan2 (vec^.yawAccess) (vec^.pitchAccess)) * 180 / pi
              in fromIntegral $ if yaw < 0
                                  then yaw + 360
                                  else yaw

angleMod :: Float -> Float
angleMod a = let b :: Int = truncate (a / shortRatio)
             in shortRatio * fromIntegral (b .&. 65535)

lerpAngles :: V3 Float -> V3 Float -> Float -> V3 Float
lerpAngles (V3 a b c) (V3 a' b' c') frac =
    V3 (lerpAngle a a' frac) (lerpAngle b b' frac) (lerpAngle c c' frac)

lerpAngle :: Float -> Float -> Float -> Float
lerpAngle a2 a1 frac =
    let a1' = if a1 - a2 > 180 then a1 - 360 else a1
        a1'' = if a1' - a2 < -180 then a1' + 360 else a1'
    in a2 + frac * (a1'' - a2)
