module Server.SVUser where

import Quake

nextServer :: Quake ()
nextServer = io (putStrLn "SVUser.nextServer") >> undefined -- TODO
