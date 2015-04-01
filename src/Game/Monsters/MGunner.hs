module Game.Monsters.MGunner where

import Quake
import QuakeState

spMonsterGunner :: EdictReference -> Quake ()
spMonsterGunner _ = io (putStrLn "MGunner.spMonsterGunner") >> undefined -- TODO
