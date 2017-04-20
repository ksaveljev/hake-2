module Game.GameCombat
    ( damage
    , radiusDamage
    ) where

import           Linear (V3)

import           Types

damage :: Ref EdictT -> Ref EdictT -> Ref EdictT -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
damage = error "GameCombat.damage" -- TODo 

radiusDamage :: Ref EdictT -> Ref EdictT -> Float -> Maybe (Ref EdictT) -> Float -> Int -> Quake ()
radiusDamage = error "GameCombat.radiusDamage" -- TODO