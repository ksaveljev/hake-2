module Sound.S
  ( initialize
  , stopAllSound
  ) where

import Types

initialize :: Quake ()
initialize = request (io (putStrLn "S.initialize IMPLEMENT ME!")) -- TODO

stopAllSound :: Quake ()
stopAllSound = request (io (putStrLn "S.stopAllSound IMPLEMENT ME!")) -- TODO
