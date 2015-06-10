module Game.GameWeapon where

import Linear (V3)

import Quake
import QuakeState

fireHit :: EdictReference -> V3 Float -> Int -> Int -> Quake Bool
fireHit _ _ _ _ = do
    io (putStrLn "PlayerWeapon.fireHit") >> undefined -- TODO
