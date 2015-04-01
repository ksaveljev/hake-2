module Game.Monsters.MBoss2 where

import Quake
import QuakeState

spMonsterBoss2 :: EdictReference -> Quake ()
spMonsterBoss2 _ = io (putStrLn "MBoss2.spMonsterBoss2") >> undefined -- TODO
