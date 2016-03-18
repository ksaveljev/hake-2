module Sound.S
  ( initialize
  , stopAllSounds
  , update
  ) where

import Types

import Linear (V3)

initialize :: Quake ()
initialize = request (io (putStrLn "S.initialize IMPLEMENT ME!")) -- TODO

stopAllSounds :: Quake ()
stopAllSounds = request (io (putStrLn "S.stopAllSound IMPLEMENT ME!")) -- TODO

update :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake ()
update _ _ _ _ = request (io (putStrLn "S.update IMPLEMENT ME!")) -- TODO