{-# LANGUAGE MultiWayIf #-}
module Util.Math3D where

import Control.Lens (preuse, ix, (^.))
import Linear (V3(..), _x, _y, _z)

import Quake
import QuakeState
                                          -- index of cmGlobals.cmMapPlanes
boxOnPlaneSize :: V3 Float -> V3 Float -> Int -> Quake Int
boxOnPlaneSize emins emaxs pIdx = do
    Just p <- preuse $ cmGlobals.cmMapPlanes.ix pIdx

    let ptype = case p^.cpType of
                  0 -> _x
                  1 -> _y
                  2 -> _z
                  _ -> undefined -- shouldn't happen
                   

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
                            _ -> undefined -- TODO: throw error

        let sides = if | dist1 >= (p^.cpDist) -> 1
                       | dist2 < (p^.cpDist) -> 2
                       | otherwise -> 0

        -- TODO: 
        -- assert(sides != 0) : "BoxOnPlaneSide(): sides == 0 bug";

        return sides
