module Game.PlayerView where

import Quake
import QuakeState

{-
- Called for each player at the end of the server frame and right after
- spawning.
-}
clientEndServerFrame :: EdictReference -> Quake ()
clientEndServerFrame _ = io (putStrLn "PlayerView.clientEndServerFrame") >> undefined -- TODO
