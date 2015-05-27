module Game.GameCombat where

import Linear (V3)

import Quake
import QuakeState

radiusDamage :: EdictReference -> EdictReference -> Float -> Maybe EdictReference -> Float -> Int -> Quake ()
radiusDamage _ _ _ _ _ _ = do
    io (putStrLn "GameCombat.radiusDamage") >> undefined -- TODO

damage :: EdictReference -> EdictReference -> EdictReference -> V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
damage _ _ _ _ _ _ _ _ _ _ = do
    io (putStrLn "GameCombat.damage") >> undefined -- TODO
