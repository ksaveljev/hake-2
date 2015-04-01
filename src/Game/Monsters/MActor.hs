module Game.Monsters.MActor where

import Quake
import QuakeState

spMiscActor :: EdictReference -> Quake ()
spMiscActor _ = io (putStrLn "MActor.spMiscActor") >> undefined -- TODO

spTargetActor :: EdictReference -> Quake ()
spTargetActor _ = io (putStrLn "MActor.spTargetActor") >> undefined -- TODO
