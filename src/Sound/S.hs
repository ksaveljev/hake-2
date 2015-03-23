module Sound.S where

import Quake

init :: Quake ()
init = io (putStrLn "S.init") >> undefined -- TODO

stopAllSounds :: Quake ()
stopAllSounds = io (putStrLn "S.stopAllSounds") >> undefined -- TODO
