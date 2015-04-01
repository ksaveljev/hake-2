module Game.Monsters.MBerserk where

import Quake
import QuakeState

spMonsterBerserk :: EdictReference -> Quake ()
spMonsterBerserk _ = io (putStrLn "MBerserk.spMonsterBerserk") >> undefined -- TODO
