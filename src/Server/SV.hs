module Server.SV where

import Quake
import QuakeState

physicsPusher :: EdictReference -> Quake ()
physicsPusher _ = io (putStrLn "SV.physicsPusher") >> undefined -- TODO

physicsNone :: EdictReference -> Quake ()
physicsNone _ = io (putStrLn "SV.physicsNone") >> undefined -- TODO

physicsNoClip :: EdictReference -> Quake ()
physicsNoClip _ = io (putStrLn "SV.physicsNoClip") >> undefined -- TODO

physicsStep :: EdictReference -> Quake ()
physicsStep _ = io (putStrLn "SV.physicsStep") >> undefined -- TODO

physicsToss :: EdictReference -> Quake ()
physicsToss _ = io (putStrLn "SV.physicsToss") >> undefined -- TODO
