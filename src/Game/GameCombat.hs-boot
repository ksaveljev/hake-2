module Game.GameCombat where

import Linear (V3)

import Quake
import QuakeState

radiusDamage :: EdictReference -> EdictReference -> Float -> Maybe EdictReference -> Float -> Int -> Quake ()

damage :: EdictReference -> EdictReference -> EdictReference -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()

canDamage :: EdictReference -> EdictReference -> Quake Bool

spawnDamage :: Int -> V3 Float -> V3 Float -> Int -> Quake ()

checkPowerArmor :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Quake Int

checkArmor :: EdictReference -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake Int

checkTeamDamage :: EdictReference -> EdictReference -> Quake Bool

killed :: EdictReference -> EdictReference -> EdictReference -> Int -> V3 Float -> Quake ()

reactToDamage :: EdictReference -> EdictReference -> Quake ()
