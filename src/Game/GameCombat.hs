module Game.GameCombat where

import Quake
import QuakeState

radiusDamage :: EdictReference -> EdictReference -> Float -> Maybe EdictReference -> Float -> Int -> Quake ()
radiusDamage _ _ _ _ _ _ = io (putStrLn "GameCombat.radiusDamage") >> undefined -- TODO
