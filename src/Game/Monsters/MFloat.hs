module Game.Monsters.MFloat where

import Quake
import QuakeState

spMonsterFloater :: EdictReference -> Quake ()
spMonsterFloater _ = io (putStrLn "MFloat.spMonsterFloater") >> undefined -- TODO
