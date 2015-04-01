module Game.Monsters.MBoss3 where

import Quake
import QuakeState

spMonsterBoss3Stand :: EdictReference -> Quake ()
spMonsterBoss3Stand _ = io (putStrLn "MBoss3.spMonsterBoss3Stand") >> undefined -- TODO
