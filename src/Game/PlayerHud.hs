module Game.PlayerHud where

import Quake
import QuakeState

moveClientToIntermission :: EdictReference -> Quake ()
moveClientToIntermission _ = do
    io (putStrLn "PlayerHud.moveClientToIntermission") >> undefined -- TODO
