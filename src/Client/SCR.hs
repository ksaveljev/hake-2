module Client.SCR where

import Quake

init :: Quake ()
init = undefined -- TODO

beginLoadingPlaque :: Quake ()
beginLoadingPlaque = io (putStrLn "SCR.beginLoadingPlaque") >> undefined -- TODO

endLoadingPlaque :: Quake ()
endLoadingPlaque = io (putStrLn "SCR.endLoadingPlaque") >> undefined -- TODO
