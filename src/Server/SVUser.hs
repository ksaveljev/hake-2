module Server.SVUser where

import Quake

nextServer :: Quake ()
nextServer = io (putStrLn "SVUser.nextServer") >> undefined -- TODO

-- Int is index of svGlobals.svServerStatic.ssClients
executeClientMessage :: Int -> Quake ()
executeClientMessage _ = io (putStrLn "SVUser.executeClientMessage") >> undefined -- TODO
