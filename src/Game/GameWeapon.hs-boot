module Game.GameWeapon where

import Linear (V3)

import Types
import QuakeState

fireHit :: EdictReference -> V3 Float -> Int -> Int -> Quake Bool

fireBlaster :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()

fireShotgun :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireRail :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Quake ()

checkDodge :: EdictReference -> V3 Float -> V3 Float -> Int -> Quake ()

fireLead :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireBullet :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireGrenade :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Float -> Float -> Quake ()

fireRocket :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Float -> Int -> Quake ()

fireBFG :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Float -> Quake ()

fireGrenade2 :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Float -> Float -> Bool -> Quake ()
