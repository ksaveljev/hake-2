module Sound.S where

import Quake

init :: Quake ()
init = io (putStrLn "S.init") >> undefined -- TODO

stopAllSounds :: Quake ()
stopAllSounds = return () -- TODO: don't want to get involved with sound system yet
    -- io (putStrLn "S.stopAllSounds") >> undefined -- TODO
