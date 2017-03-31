module Game.GameCombat ( damage
                       , radiusDamage
                       ) where

import Linear (V3)

import Types
import QuakeState

radiusDamage :: Ref EdictT -> Ref EdictT -> Float -> Maybe (Ref EdictT) -> Float -> Int -> Quake ()

damage :: Ref EdictT -> Ref EdictT -> Ref EdictT -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
