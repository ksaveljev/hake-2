module Game.PlayerHud where

import Quake
import QuakeState

moveClientToIntermission :: EdictReference -> Quake ()
moveClientToIntermission _ = do
    io (putStrLn "PlayerHud.moveClientToIntermission") >> undefined -- TODO

setStats :: EdictReference -> Quake ()
setStats _ = do
    io (putStrLn "PlayerHud.setStats") >> undefined -- TODO

checkChaseStats :: EdictReference -> Quake ()
checkChaseStats _ = do
    io (putStrLn "PlayerHud.checkChaseStats") >> undefined -- TODO
