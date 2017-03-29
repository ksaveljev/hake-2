module Game.GameCombat ( damage
                       , radiusDamage
                       ) where

import Linear (V3)

import Types
import QuakeState

radiusDamage :: EdictReference -> EdictReference -> Float -> Maybe EdictReference -> Float -> Int -> Quake ()

damage :: EdictReference -> EdictReference -> EdictReference -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()