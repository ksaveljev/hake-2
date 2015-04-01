module Game.Monsters.MHover where

import Quake
import QuakeState

spMonsterHover :: EdictReference -> Quake ()
spMonsterHover _ = io (putStrLn "MHover.spMonsterHover") >> undefined -- TODO
