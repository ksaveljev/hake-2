module Client.Console where

import Quake

init :: Quake ()
init = io (putStrLn "Console.init") >> undefined -- TODO
