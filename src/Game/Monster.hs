module Game.Monster where

import Quake
import QuakeState

monsterStart :: EdictReference -> Quake Bool
monsterStart _ = io (putStrLn "Monster.monsterStart") >> undefined -- TODO
