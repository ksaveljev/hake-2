module Game.GameWeapon where

import Linear (V3)

import Types
import QuakeState

fireHit :: Ref EdictT -> V3 Float -> Int -> Int -> Quake Bool

fireBlaster :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Bool -> Quake ()

fireShotgun :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireRail :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Quake ()

checkDodge :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Quake ()

fireLead :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireBullet :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()

fireGrenade :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Float -> Quake ()

fireRocket :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Int -> Quake ()

fireBFG :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Quake ()

fireGrenade2 :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Float -> Float -> Bool -> Quake ()
