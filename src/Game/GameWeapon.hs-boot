module Game.GameWeapon where

import Linear (V3)

import Quake
import QuakeState

fireHit :: EdictReference -> V3 Float -> Int -> Int -> Quake Bool

fireBlaster :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()

fireShotgun :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireRail :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Quake ()

checkDodge :: EdictReference -> V3 Float -> V3 Float -> Int -> Quake ()

fireLead :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireBullet :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()
