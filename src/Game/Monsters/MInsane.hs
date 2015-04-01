module Game.Monsters.MInsane where

import Quake
import QuakeState

spMiscInsane :: EdictReference -> Quake ()
spMiscInsane _ = io (putStrLn "MInsane.spMiscInsane") >> undefined -- TODO
